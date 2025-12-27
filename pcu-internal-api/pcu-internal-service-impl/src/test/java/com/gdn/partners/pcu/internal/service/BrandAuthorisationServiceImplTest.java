package com.gdn.partners.pcu.internal.service;

import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.verify;

import java.io.File;
import java.io.IOException;
import java.time.Duration;
import java.time.Instant;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.gdn.partners.pcu.internal.client.model.request.BrandAuthDeleteRequest;
import com.gdn.partners.pcu.internal.service.impl.config.KafkaPublisher;
import com.gdn.partners.pcu.internal.client.model.response.BrandAuthFilterResponse;
import com.gdn.x.businesspartner.dto.CompanyDTO;
import com.gdn.x.businesspartner.dto.ProfileResponse;
import org.apache.commons.io.FileUtils;
import org.apache.commons.lang.StringUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.data.domain.Page;
import org.springframework.mock.web.MockMultipartFile;

import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.common.web.wrapper.response.PageMetaData;
import com.gdn.mta.bulk.dto.BulkBrandAuthUploadModel;
import com.gdn.mta.bulk.dto.product.constant.DomainEventName;
import com.gdn.partners.core.exception.ApplicationException;
import com.gdn.partners.pcu.internal.client.feign.PCBFeign;
import com.gdn.partners.pcu.internal.properties.SystemParameterProperties;
import com.gdn.partners.pcu.internal.service.impl.BrandAuthServiceImpl;
import com.gdn.partners.pcu.internal.service.impl.exception.ClientException;
import com.gdn.partners.pcu.internal.service.impl.helper.RequestHelper;
import com.gdn.partners.pcu.internal.web.model.request.BrandAuthWebRequest;
import com.gdn.partners.pcu.internal.web.model.response.BrandAuthCreateWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.BrandAuthFilterWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.BrandAuthorisationDetailWebResponse;
import com.gdn.x.businesspartner.dto.MerchantNameResponse;
import com.gdn.x.productcategorybase.BrandAuthorisationStatus;
import com.gdn.x.productcategorybase.dto.brand.BrandAuthCreateRequest;
import com.gdn.x.productcategorybase.dto.brand.BrandAuthCreateResponse;
import com.gdn.x.productcategorybase.dto.brand.BrandAuthHistoryRequest;
import com.gdn.x.productcategorybase.dto.brand.BrandAuthHistoryResponse;
import com.gdn.x.productcategorybase.dto.request.BrandAuthUpdateRequest;
import com.gdn.x.productcategorybase.dto.response.BrandAuthorisationDetailResponse;

@ExtendWith(MockitoExtension.class)
public class BrandAuthorisationServiceImplTest {

  private static final String DEFAULT_SELLER_CODE = "BR-0001-0001";
  private static final String DEFAULT_BRAND_CODE = "BR-0001";
  private static final String DEFAULT_REQUEST_ID = "requestId";
  private static final String DEFAULT_SELLER_NAME = "seller name";
  private static final String DEFAULT_BRAND_NAME = "Brand name";
  private static final String DEFAULT_STORE_ID = "10001";
  private static final String DEFAULT_USERNAME = "username";
  private static final String SOURCE_PATH = "SOURCE_PATH";
  private static final String DEFAULT_BRAND_AUTH_DOC = "doc1.pdf";
  private static final String SLASH_SEPARATOR = "/";
  private static final String DASH_SEPARATOR = "-";
  private static final String OLD_VALUE_AUTH = "Inactive";
  private static final String NEW_VALUE_AUTH = "Active";
  private static final String ACTIVITY_AUTH = "Change status";
  private static final String FILE_NAME = "originalFile.pdf";
  private static final String VALID_FILE_EXTENSION = ".pdf,.docx,.doc";
  private static final ClassLoader CLASS_LOADER = ClassLoader.getSystemClassLoader();
  private static final String BASE_DIRECTORY = CLASS_LOADER.getResource(StringUtils.EMPTY).getPath();
  private static final String PATH = BASE_DIRECTORY + "path";
  private static final String FILE = "/filestore/originalFilename.xls";
  private static final int PAGE = 0;
  private static final int SIZE = 10;
  private static final byte[] fileContent = new byte[]{-1, -40, -20, -10};
  private static final String ORIGINAL_FILENAME = "originalFilename.xls";
  private static final String TYPE = "BRAND_AUTH_ADD";
  private MockMultipartFile multipartFile;

  @InjectMocks
  BrandAuthServiceImpl brandAuthService;

  @Mock
  PCBFeign pcbFeign;

  @Mock
  BPService bpService;

  @Mock
  FileStorageService fileStorageService;
  @Mock
  private KafkaPublisher kafkaPublisher;

  @Mock
  private SystemParameterProperties systemParameterProperties;
  @Captor
  private ArgumentCaptor<BulkBrandAuthUploadModel> bulkBrandAuthUploadModelArgumentCaptor;

  private GdnRestSingleResponse<BrandAuthorisationDetailResponse> detailResponse =
    new GdnRestSingleResponse<>();
  private BrandAuthorisationDetailResponse brandAuthorisationDetailResponse = new BrandAuthorisationDetailResponse();
  private GdnRestSingleResponse<BrandAuthCreateResponse> authCreateResponseGdnRestSingleResponse
    = new GdnRestSingleResponse<>();;
  private BrandAuthCreateResponse brandAuthCreateResponse;
  private BrandAuthCreateRequest brandAuthCreateRequest;
  private BrandAuthUpdateRequest brandAuthUpdateRequest;
  private BrandAuthHistoryRequest brandAuthHistoryRequest;
  private BrandAuthHistoryResponse brandAuthHistoryResponse;
  private List<BrandAuthHistoryResponse> brandAuthHistoryResponseList = new ArrayList<>();
  private GdnRestListResponse<BrandAuthHistoryResponse> historyResponse = new GdnRestListResponse<>();
  private List<BrandAuthDeleteRequest> brandAuthDeleteRequestList = new ArrayList<>();

  private Map<String, ProfileResponse> profileResponseMap;

  @Mock
  private BusinessPartnerService businessPartnerService;

  @BeforeEach
  public void setUp() throws Exception {
    brandAuthorisationDetailResponse.setAuthorisationStatus(BrandAuthorisationStatus.ACTIVE);
    brandAuthorisationDetailResponse.setSellerId(DEFAULT_SELLER_CODE);
    brandAuthorisationDetailResponse.setBrandName(DEFAULT_BRAND_NAME);
    brandAuthorisationDetailResponse.setAuthStartDate(
      Date.from(Instant.now().minus(Duration.ofDays(1))));
    brandAuthorisationDetailResponse.setAuthExpireDate(
      Date.from(Instant.now().plus(Duration.ofDays(1))));
    detailResponse.setValue(brandAuthorisationDetailResponse);
    detailResponse.setRequestId(DEFAULT_REQUEST_ID);
    Date startDate = new Date();
    Calendar calendar = Calendar.getInstance();
    calendar.setTime(startDate);
    calendar.add(Calendar.DATE, 1);
    startDate = calendar.getTime();
    calendar.add(Calendar.DATE, 10);
    Date endDate = calendar.getTime();
    brandAuthCreateRequest =
      BrandAuthCreateRequest.builder().brandCode(DEFAULT_BRAND_CODE).brandName(DEFAULT_BRAND_NAME)
        .sellerCode(DEFAULT_SELLER_CODE).authStartDate(startDate).authExpireDate(endDate)
        .authorisationStatus("ACTIVE").documentLinks(Arrays.asList(DEFAULT_BRAND_AUTH_DOC)).build();
    brandAuthUpdateRequest = BrandAuthUpdateRequest.builder().brandCode(DEFAULT_BRAND_CODE)
        .sellerCode(DEFAULT_SELLER_CODE).authStartDate(startDate).authExpireDate(endDate)
        .authorisationStatus("ACTIVE").documentLinks(Arrays.asList(DEFAULT_BRAND_AUTH_DOC)).build();
    brandAuthCreateResponse =
      BrandAuthCreateResponse.builder().brandCode(DEFAULT_BRAND_CODE).build();
    authCreateResponseGdnRestSingleResponse.setValue(brandAuthCreateResponse);
    authCreateResponseGdnRestSingleResponse.setRequestId(DEFAULT_REQUEST_ID);
    authCreateResponseGdnRestSingleResponse.setSuccess(true);

    brandAuthHistoryResponse = new BrandAuthHistoryResponse();
    brandAuthHistoryResponse.setBrandCode(DEFAULT_BRAND_CODE);
    brandAuthHistoryResponse.setSellerCode(DEFAULT_SELLER_CODE);
    brandAuthHistoryResponse.setActivity(ACTIVITY_AUTH);
    brandAuthHistoryResponse.setOldStatus(OLD_VALUE_AUTH);
    brandAuthHistoryResponse.setNewStatus(NEW_VALUE_AUTH);

    brandAuthHistoryRequest = new BrandAuthHistoryRequest();
    brandAuthHistoryRequest.setSellerCode(DEFAULT_SELLER_CODE);
    brandAuthHistoryRequest.setBrandCode(DEFAULT_BRAND_CODE);
    brandAuthHistoryResponseList.add(brandAuthHistoryResponse);
    historyResponse.setSuccess(true);
    historyResponse.setContent(brandAuthHistoryResponseList);

    brandAuthDeleteRequestList.add(
      new BrandAuthDeleteRequest(DEFAULT_SELLER_CODE, DEFAULT_BRAND_CODE, "id", "status"));

    profileResponseMap = new HashMap<>();
    profileResponseMap.put(DEFAULT_SELLER_CODE, ProfileResponse.builder()
      .company(CompanyDTO.builder().businessPartnerName(DEFAULT_SELLER_NAME).build()).build());
  }

  @AfterEach
  public void tearDown() throws Exception {
    Mockito.verifyNoMoreInteractions(pcbFeign);
    Mockito.verifyNoMoreInteractions(bpService);
    Mockito.verifyNoMoreInteractions(businessPartnerService);
    Mockito.verifyNoMoreInteractions(systemParameterProperties);
    Mockito.verifyNoMoreInteractions(fileStorageService);
  }

  @Test
  public void getBrandAuthDetailBySellerCodeAndBrandCodeTest() throws Exception {
    this.detailResponse.setSuccess(true);
    Mockito.when(
      this.pcbFeign.getBrandAuthorisationDetailByCode(DEFAULT_STORE_ID, DEFAULT_SELLER_CODE,
        DEFAULT_BRAND_CODE)).thenReturn(detailResponse);
    Mockito.when(this.businessPartnerService.getBusinessPartnerNameByCode(DEFAULT_SELLER_CODE))
      .thenReturn(DEFAULT_SELLER_NAME);
    BrandAuthorisationDetailWebResponse response =
      this.brandAuthService.getBrandAuthDetailBySellerCodeAndBrandCode(DEFAULT_STORE_ID,
        DEFAULT_SELLER_CODE, DEFAULT_BRAND_CODE);
    Mockito.verify(pcbFeign)
      .getBrandAuthorisationDetailByCode(DEFAULT_STORE_ID, DEFAULT_SELLER_CODE, DEFAULT_BRAND_CODE);
    Mockito.verify(businessPartnerService).getBusinessPartnerNameByCode(DEFAULT_SELLER_CODE);
    Assertions.assertEquals(DEFAULT_SELLER_NAME, response.getSellerName());

  }

  @Test
  public void getBrandAuthDetailBySellerCodeAndBrandCodeExceptionTest() throws Exception {
    this.detailResponse.setSuccess(true);
    Mockito.when(
      this.pcbFeign.getBrandAuthorisationDetailByCode(DEFAULT_STORE_ID, DEFAULT_SELLER_CODE,
        DEFAULT_BRAND_CODE)).thenReturn(detailResponse);
    Mockito.doThrow(ApplicationRuntimeException.class).when(this.businessPartnerService)
      .getBusinessPartnerNameByCode(DEFAULT_SELLER_CODE);
    try {
      this.brandAuthService.getBrandAuthDetailBySellerCodeAndBrandCode(DEFAULT_STORE_ID,
        DEFAULT_SELLER_CODE, DEFAULT_BRAND_CODE);
    } catch (ApplicationRuntimeException ex){
      Assertions.assertNotNull(ex);
    } finally {
      Mockito.verify(pcbFeign)
        .getBrandAuthorisationDetailByCode(DEFAULT_STORE_ID, DEFAULT_SELLER_CODE, DEFAULT_BRAND_CODE);
      Mockito.verify(businessPartnerService).getBusinessPartnerNameByCode(DEFAULT_SELLER_CODE);
    }
  }

  @Test
  public void deleteTest() throws Exception {
    Mockito.when(
        pcbFeign.delete(DEFAULT_STORE_ID, DEFAULT_USERNAME, brandAuthDeleteRequestList))
      .thenReturn(new GdnBaseRestResponse(null,null, Boolean.TRUE,DEFAULT_REQUEST_ID));
    this.brandAuthService.delete(DEFAULT_STORE_ID, DEFAULT_USERNAME, brandAuthDeleteRequestList);
    Mockito.verify(pcbFeign)
      .delete(DEFAULT_STORE_ID, DEFAULT_USERNAME, brandAuthDeleteRequestList);
  }

  @Test
  public void deleteExceptionTest() throws Exception {
    Mockito.when(
        pcbFeign.delete(DEFAULT_STORE_ID, DEFAULT_USERNAME, brandAuthDeleteRequestList))
      .thenReturn(new GdnBaseRestResponse("", "", Boolean.FALSE, DEFAULT_REQUEST_ID));
    try {
      this.brandAuthService.delete(DEFAULT_STORE_ID, DEFAULT_USERNAME, brandAuthDeleteRequestList);
    } catch (ClientException ex){
      Assertions.assertNotNull(ex);
    } finally {
      Mockito.verify(pcbFeign)
        .delete(DEFAULT_STORE_ID, DEFAULT_USERNAME, brandAuthDeleteRequestList);
    }
  }

  @Test
  public void deleteApplicationRuntimeExceptionTest() throws Exception {
    Mockito.doThrow(ApplicationRuntimeException.class).when(this.pcbFeign)
      .delete(DEFAULT_STORE_ID, DEFAULT_USERNAME, brandAuthDeleteRequestList);
    try {
      this.brandAuthService.delete(DEFAULT_STORE_ID, DEFAULT_USERNAME, brandAuthDeleteRequestList);
    } catch (ApplicationRuntimeException ex){
      Assertions.assertNotNull(ex);
    } finally {
      Mockito.verify(pcbFeign)
        .delete(DEFAULT_STORE_ID, DEFAULT_USERNAME, brandAuthDeleteRequestList);
    }
  }

  @Test
  public void getAuthorisationsTest() {
    BrandAuthFilterResponse brandAuthFilterResponse = new BrandAuthFilterResponse();
    brandAuthFilterResponse.setBrandName(DEFAULT_BRAND_NAME);
    brandAuthFilterResponse.setSellerCode(DEFAULT_SELLER_CODE);
    MerchantNameResponse merchantNameResponse = new MerchantNameResponse();
    merchantNameResponse.setMerchantCode(DEFAULT_SELLER_CODE);
    merchantNameResponse.setMerchantName(DEFAULT_SELLER_CODE);
    Mockito.when(pcbFeign.getAuthorisations(Mockito.any(), Mockito.anyInt(), Mockito.anyInt())).thenReturn(
        new GdnRestListResponse<>(new ArrayList<>(Arrays.asList(brandAuthFilterResponse)), new PageMetaData(1, 10, 10),
            DEFAULT_REQUEST_ID));
    Mockito.when(bpService.getProfileResponseMap(Mockito.any())).thenReturn(profileResponseMap);
    Page<BrandAuthFilterWebResponse> authorisations =
        this.brandAuthService.getAuthorisations(new BrandAuthWebRequest(), 0, 10);
    Mockito.verify(pcbFeign).getAuthorisations(Mockito.any(), Mockito.anyInt(), Mockito.anyInt());
    Mockito.verify(bpService).getProfileResponseMap(Mockito.anyList());
    Assertions.assertNotNull(authorisations);
    Assertions.assertEquals(1, authorisations.getContent().size());
    Assertions.assertEquals(DEFAULT_SELLER_NAME, authorisations.getContent().get(0).getSellerName());
  }

  @Test
  public void getAuthorisationsSellerNameNotFoundTest() {
    BrandAuthFilterResponse brandAuthFilterResponse = new BrandAuthFilterResponse();
    brandAuthFilterResponse.setBrandName(DEFAULT_BRAND_NAME);
    brandAuthFilterResponse.setSellerCode(DEFAULT_SELLER_CODE);
    MerchantNameResponse merchantNameResponse = new MerchantNameResponse();
    merchantNameResponse.setMerchantCode(DEFAULT_SELLER_CODE);
    merchantNameResponse.setMerchantName(DEFAULT_SELLER_CODE);
    Mockito.when(pcbFeign.getAuthorisations(Mockito.any(), Mockito.anyInt(), Mockito.anyInt())).thenReturn(
        new GdnRestListResponse<>(new ArrayList<>(Arrays.asList(brandAuthFilterResponse)), new PageMetaData(1, 10, 10),
            DEFAULT_REQUEST_ID));
    Mockito.when(bpService.getProfileResponseMap(Mockito.any())).thenReturn(profileResponseMap);
    Page<BrandAuthFilterWebResponse> authorisations =
        this.brandAuthService.getAuthorisations(new BrandAuthWebRequest(), 0, 10);
    Mockito.verify(pcbFeign).getAuthorisations(Mockito.any(), Mockito.anyInt(), Mockito.anyInt());
    Mockito.verify(bpService).getProfileResponseMap(Mockito.anyList());
    Assertions.assertNotNull(authorisations);
    Assertions.assertEquals(1, authorisations.getContent().size());
    Assertions.assertEquals(1, authorisations.getContent().size());
    Assertions.assertEquals(DEFAULT_SELLER_NAME, authorisations.getContent().get(0).getSellerName());
  }
  private void mockFile(String filePath) {
    File file = new File(filePath);
    file.mkdirs();
  }

  @Test
  public void createAuthWithoutDocTest() {
    brandAuthCreateRequest.setDocumentLinks(Arrays.asList(""));
    try {
      BrandAuthCreateWebResponse webResponse =
          brandAuthService.create(DEFAULT_STORE_ID, DEFAULT_USERNAME, DEFAULT_REQUEST_ID,
              brandAuthCreateRequest);
      Assertions.assertEquals(DEFAULT_BRAND_CODE, webResponse.getBrandCode());
    } catch (ApplicationRuntimeException ex){
      Assertions.assertNotNull(ex);
    }
  }

  @Test
  public void createAuthWithoutDocumentListEmptyTest() {
    brandAuthCreateRequest.setDocumentLinks(new ArrayList<>());
    Mockito.when(pcbFeign
        .createBrandAuthorisation(DEFAULT_STORE_ID, DEFAULT_REQUEST_ID, DEFAULT_USERNAME,
            brandAuthCreateRequest)).thenReturn(authCreateResponseGdnRestSingleResponse);
    BrandAuthCreateWebResponse webResponse = brandAuthService
        .create(DEFAULT_STORE_ID, DEFAULT_USERNAME, DEFAULT_REQUEST_ID, brandAuthCreateRequest);
    Mockito.verify(pcbFeign)
        .createBrandAuthorisation(DEFAULT_STORE_ID, DEFAULT_REQUEST_ID, DEFAULT_USERNAME,
            brandAuthCreateRequest);
    Assertions.assertEquals(DEFAULT_BRAND_CODE, webResponse.getBrandCode());

  }

  @Test
  public void createAuthWitDocTest() throws IOException {
    mockFile(
      SOURCE_PATH + SLASH_SEPARATOR + DEFAULT_BRAND_CODE + DASH_SEPARATOR + DEFAULT_SELLER_CODE
        + SLASH_SEPARATOR + DEFAULT_BRAND_AUTH_DOC);
    Mockito.when(pcbFeign
      .createBrandAuthorisation(DEFAULT_STORE_ID, DEFAULT_REQUEST_ID, DEFAULT_USERNAME,
        brandAuthCreateRequest)).thenReturn(authCreateResponseGdnRestSingleResponse);
    BrandAuthCreateWebResponse webResponse = brandAuthService
      .create(DEFAULT_STORE_ID, DEFAULT_USERNAME, DEFAULT_REQUEST_ID, brandAuthCreateRequest);
    Mockito.verify(pcbFeign)
      .createBrandAuthorisation(DEFAULT_STORE_ID, DEFAULT_REQUEST_ID, DEFAULT_USERNAME,
        brandAuthCreateRequest);
    Mockito.verify(fileStorageService)
      .checkIfFileExisting(Mockito.anyString(), Mockito.anyString(), Mockito.anyString());
    Assertions.assertEquals(DEFAULT_BRAND_CODE, webResponse.getBrandCode());
  }

  @Test
  public void updateAuthWitDocTest() throws IOException {
    mockFile(
        SOURCE_PATH + SLASH_SEPARATOR + DEFAULT_BRAND_CODE + DASH_SEPARATOR + DEFAULT_SELLER_CODE
            + SLASH_SEPARATOR + DEFAULT_BRAND_AUTH_DOC);
    Mockito.when(pcbFeign
        .updateBrandAuthorisation(DEFAULT_STORE_ID, DEFAULT_REQUEST_ID, DEFAULT_USERNAME,
            brandAuthUpdateRequest)).thenReturn(new GdnBaseRestResponse(true));
    brandAuthService
        .update(DEFAULT_STORE_ID, DEFAULT_USERNAME, DEFAULT_REQUEST_ID, brandAuthUpdateRequest);
    Mockito.verify(pcbFeign)
        .updateBrandAuthorisation(DEFAULT_STORE_ID, DEFAULT_REQUEST_ID, DEFAULT_USERNAME,
            brandAuthUpdateRequest);
    Mockito.verify(fileStorageService)
      .checkIfFileExisting(Mockito.anyString(), Mockito.anyString(), Mockito.anyString());
  }

  @Test
  public void getBrandAuthHistoryTest(){
    Mockito.when(this.pcbFeign.getBrandAuthHistory(DEFAULT_STORE_ID,DEFAULT_REQUEST_ID,DEFAULT_USERNAME,
      brandAuthHistoryRequest,PAGE,SIZE))
      .thenReturn(historyResponse);
    GdnRestListResponse<BrandAuthHistoryResponse> response
     =  brandAuthService.getBrandAuthHistory(DEFAULT_STORE_ID,DEFAULT_REQUEST_ID,DEFAULT_USERNAME,
      brandAuthHistoryRequest,PAGE,SIZE);
    Mockito.verify(this.pcbFeign).getBrandAuthHistory(DEFAULT_STORE_ID,DEFAULT_REQUEST_ID,
      DEFAULT_USERNAME,
      brandAuthHistoryRequest,PAGE,SIZE);
    Assertions.assertEquals(NEW_VALUE_AUTH, response.getContent().get(0).getNewStatus());
    Assertions.assertTrue(response.isSuccess());
    Assertions.assertEquals(OLD_VALUE_AUTH, response.getContent().get(0).getOldStatus());
  }
  @Test
  public void uploadBrandAuthDocTest() throws Exception {
    Mockito.when(this.systemParameterProperties.getBrandAuthDocTypes())
      .thenReturn(VALID_FILE_EXTENSION);
    brandAuthService.uploadBrandAuthDoc(FILE_NAME, DEFAULT_BRAND_CODE, DEFAULT_SELLER_CODE,
      fileContent);
    Mockito.verify(fileStorageService)
      .uploadBrandAuthDoc(FILE_NAME, DEFAULT_BRAND_CODE, DEFAULT_SELLER_CODE, fileContent);
    Mockito.verify(systemParameterProperties).getBrandAuthDocTypes();
  }

  @Test
  public void uploadBrandAuthDocInvalidTest() throws Exception {
    Mockito.when(this.systemParameterProperties.getBrandAuthDocTypes())
      .thenReturn(VALID_FILE_EXTENSION);
    try {
      brandAuthService.uploadBrandAuthDoc("invalid_file.xlsx", DEFAULT_BRAND_CODE,
        DEFAULT_SELLER_CODE, fileContent);
    } catch (ApplicationException ex){
      Assertions.assertNotNull(ex);
    } finally {
      Mockito.verify(systemParameterProperties).getBrandAuthDocTypes();
    }
  }

  @Test
  public void saveBulkBrandAuthFileTest() throws Exception {
    mockFile(PATH + FILE);
    multipartFile = new MockMultipartFile("request", ORIGINAL_FILENAME, "application/vnd.ms-excel",
        FileUtils.readFileToByteArray(new File(PATH + FILE)));
    Mockito.when(fileStorageService.uploadFilePath(Mockito.any(), Mockito.anyString(), Mockito.anyString()))
        .thenReturn(PATH);
    brandAuthService.saveBulkBrandAuthFile(multipartFile, TYPE, DEFAULT_REQUEST_ID, DEFAULT_STORE_ID, DEFAULT_USERNAME);
    Mockito.verify(fileStorageService).uploadFilePath(Mockito.any(), Mockito.anyString(), Mockito.anyString());
    verify(kafkaPublisher).send(eq(DomainEventName.BULK_BRAND_AUTH_UPLOAD_EVENT), anyString(),
        bulkBrandAuthUploadModelArgumentCaptor.capture());
    Assertions.assertNotNull(new File(PATH + DEFAULT_REQUEST_ID + ORIGINAL_FILENAME));
    Assertions.assertNotNull(bulkBrandAuthUploadModelArgumentCaptor.getValue());
    Assertions.assertEquals(TYPE, bulkBrandAuthUploadModelArgumentCaptor.getValue().getBulkProcessType());
    Assertions.assertEquals(DEFAULT_REQUEST_ID, bulkBrandAuthUploadModelArgumentCaptor.getValue().getRequestId());
    Assertions.assertEquals(DEFAULT_STORE_ID, bulkBrandAuthUploadModelArgumentCaptor.getValue().getStoreId());
    Assertions.assertEquals(DEFAULT_USERNAME, bulkBrandAuthUploadModelArgumentCaptor.getValue().getCreatedBy());
  }

  @Test
  public void brandAuthDownloadAllTest() {
    BrandAuthWebRequest brandAuthWebRequest = new BrandAuthWebRequest();
    brandAuthService.brandAuthDownloadAll(DEFAULT_USERNAME, brandAuthWebRequest);
    Mockito.verify(kafkaPublisher).send(DomainEventName.BULK_DOWNLOAD_ALL_EVENT, DEFAULT_USERNAME,
        RequestHelper.toBulkBrandAuthDownloadRequest(DEFAULT_USERNAME, brandAuthWebRequest));
  }

}