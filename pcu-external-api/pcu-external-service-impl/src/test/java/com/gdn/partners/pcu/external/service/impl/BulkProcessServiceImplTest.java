package com.gdn.partners.pcu.external.service.impl;

import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.common.web.wrapper.response.PageMetaData;
import com.gdn.mta.bulk.dto.BulkDownloadFileContentResponse;
import com.gdn.mta.bulk.dto.BulkPendingRequestsResponse;
import com.gdn.mta.bulk.dto.BulkProcessDeleteOfflineItemRequest;
import com.gdn.mta.bulk.dto.BulkProcessNotesResponse;
import com.gdn.mta.bulk.dto.BulkProcessResponse;
import com.gdn.mta.bulk.dto.BulkProcessStatusListingResponse;
import com.gdn.mta.bulk.dto.BulkProcessType;
import com.gdn.mta.bulk.dto.BulkProcessUpdateRequest;
import com.gdn.mta.bulk.dto.BulkProcessUploadRequest;
import com.gdn.mta.bulk.dto.BulkProcessUpsertOfflineItemRequest;
import com.gdn.mta.bulk.dto.BulkProcessV2Request;
import com.gdn.mta.bulk.dto.PickupPointCodesRequestDTO;
import com.gdn.mta.bulk.dto.SystemParameterConfigResponse;
import com.gdn.mta.bulk.dto.UnifiedBulkDownloadResponse;
import com.gdn.mta.bulk.dto.WholeSaleCountResponse;
import com.gdn.mta.bulk.dto.product.BulkProcessSubjectToVatRequest;
import com.gdn.mta.product.util.GdnRestSimpleResponse;
import com.gdn.partners.core.security.Credential;
import com.gdn.partners.pcu.external.client.feign.XBulkFeign;
import com.gdn.partners.pcu.external.client.helper.MandatoryParameterHelper;
import com.gdn.partners.pcu.external.model.Accessibilty;
import com.gdn.partners.pcu.external.model.Constants;
import com.gdn.partners.pcu.external.model.ErrorMessages;
import com.gdn.partners.pcu.external.properties.GCSProperties;
import com.gdn.partners.pcu.external.service.BusinessPartnerService;
import com.gdn.partners.pcu.external.service.UserPicService;
import com.gdn.partners.pcu.external.service.impl.exception.ClientException;
import com.gdn.partners.pcu.external.service.impl.exception.ValidationException;
import com.gdn.partners.pcu.external.service.impl.helper.RequestHelper;
import com.gdn.partners.pcu.external.web.model.request.BulkBasicInfoRequest;
import com.gdn.partners.pcu.external.web.model.request.BulkBasicInfoWebRequest;
import com.gdn.partners.pcu.external.web.model.response.BulkPendingRequestsWebResponse;
import com.gdn.partners.pcu.external.web.model.response.BulkProcessStatusListingWebResponse;
import com.gdn.partners.pcu.external.web.model.response.PromoUpdateProductResponse;
import com.gdn.partners.pcu.external.web.model.response.WholesaleCountWebResponse;
import com.gdn.x.businesspartner.dto.CompanyDTO;
import com.gdn.x.businesspartner.dto.ProfileResponse;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.io.IOUtils;
import org.apache.commons.lang.StringUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockedStatic;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.data.domain.Page;
import org.springframework.mock.web.MockHttpServletResponse;
import org.springframework.mock.web.MockMultipartFile;
import org.springframework.test.util.ReflectionTestUtils;
import org.springframework.web.multipart.MultipartFile;

import java.io.File;
import java.io.FileInputStream;
import java.lang.reflect.Field;
import java.lang.reflect.Modifier;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Optional;

import static com.gdn.partners.pcu.external.model.Constants.BULK_UPSERT;
import static com.gdn.partners.pcu.external.model.Constants.CHANNEL_ID;
import static com.gdn.partners.pcu.external.model.Constants.CLIENT_ID;
import static com.gdn.partners.pcu.external.model.Constants.INACTIVE_STATUS;
import static com.gdn.partners.pcu.external.model.Constants.REQUEST_ID;
import static com.gdn.partners.pcu.external.model.Constants.STORE_ID;
import static com.gdn.partners.pcu.external.model.Constants.USER_NAME;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;


public class BulkProcessServiceImplTest {

  private static final String BUSINESS_PARTNER_CODE = "businessPartnerCode";
  private static final String BULK_PROCESS_CODE = "bulpProcessCode";
  private static final String USERNAME = "username";
  private static final String DUMMY_FILE_NAME = "dummy-excel.xls";
  private static final String FILE_FOLDER = "Product";
  private static final String PRIVILEGED_EDIT_O2O = "isPrivilegedToEditO2O";
  private static final String BULK_PROCESS_TYPE = "ProductLevel3";
  private static final String ZIP_FILE = "dummy.zip";
  private static final String EXCEL_FILE = "dummy.xlsx";
  private static final String EXCEL_FILE_XLSM = "dummy.xlsm";
  private static final String DUMMY_EXCEL_FILE = "DummyExcelFile.xlsx";
  private static final String UNAUTHORIZE_ERROR = "Unauthorize access :You are not authorized";
  private static final String DEFAULT_ITEM_NAME = "apple iphone 6s";
  private static final String DEFAULT_ITEM_SKU = "itemSKU";
  private static final String COMMA_DELIMITER = ", ";
  private static final String DEFAULT_PROCESS_CODE = "defaultBulkProcessCode";
  private static final String IS_ONLY_EXTERNAL_USER = "isOnlyExternalUser";
  private static final String CLIENT_HOST = "client-host";
  private static final String EXCEL_UPSERT_FILE = "dummyUpsert.xlsx";
  private static final String PICKUP_POINT_CODE = "pickupPointCode";

  @Mock
  private XBulkFeign xBulkFeign;

  @Mock
  private BusinessPartnerService businessPartnerService;

  @Mock
  private MandatoryParameterHelper mandatoryParameterHelper;

  @Mock
  private UserPicService userPicService;

  @InjectMocks
  private BulkProcessServiceImpl bulkProcessService;

  private ProfileResponse profileResponse;
  private BulkProcessUpdateRequest bulkProcessUpdateRequest;
  private GdnBaseRestResponse gdnBaseRestResponse;
  private Map<String, Boolean> privilegeMap = new HashMap<>();
  private List<String> fileNames = new ArrayList<>();
  private BulkProcessNotesResponse bulkProcessNotesResponse = new BulkProcessNotesResponse();
  private BulkPendingRequestsResponse pendingRequestsResponse;
  private BulkDownloadFileContentResponse bulkDownloadFileContentResponse;
  private MockHttpServletResponse httpServletResponse;
  private UnifiedBulkDownloadResponse unifiedBulkDownloadResponse;
  private SystemParameterConfigResponse systemParameterConfigResponse;
  private BulkProcessDeleteOfflineItemRequest bulkProcessDeleteOfflineItemRequest;
  private Map<String,String> externalFiles = new HashMap<>();

  @Captor
  private ArgumentCaptor<BulkProcessUpdateRequest> bulkProcessUpdateRequestArgumentCaptor;
  @Captor
  private ArgumentCaptor<BulkProcessV2Request> bulkProcessV2RequestArgumentCaptor;

  @Captor
  private ArgumentCaptor<BulkBasicInfoRequest> bulkBasicInfoRequestArgumentCaptor;


  @Captor
  private ArgumentCaptor<BulkProcessSubjectToVatRequest> bulkProcessSubjectToVatRequestArgumentCaptor;

  @Captor
  private ArgumentCaptor<BulkProcessUploadRequest> bulkProcessUploadRequestArgumentCaptor;

  @Captor
  private ArgumentCaptor<BulkProcessUpsertOfflineItemRequest> bulkProcessUpsertOfflineItemRequestArgumentCaptor;

  @Captor
  private ArgumentCaptor<BulkProcessDeleteOfflineItemRequest> bulkProcessDeleteOfflineItemRequestArgumentCaptor;

  @Mock
  private GCSProperties gcsProperties;

  @Mock
  private GCSServiceImpl gcsConfig;

  @Mock
  private FileStorageServiceImpl fileStorageService;

  @BeforeEach
  public void init() throws Exception {
    MockitoAnnotations.initMocks(this);
    profileResponse = new ProfileResponse();
    CompanyDTO company = new CompanyDTO();
    company.setOfflineToOnlineFlag(true);
    company.setCncActivated(true);
    profileResponse.setCompany(company);
    profileResponse.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    profileResponse.setMerchantStatus("ACTIVE");

    privilegeMap.put(PRIVILEGED_EDIT_O2O, false);

    bulkProcessUpdateRequest = new BulkProcessUpdateRequest();
    bulkProcessUpdateRequest.setBulkProcessType(BULK_PROCESS_TYPE);
    bulkProcessUpdateRequest.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    bulkProcessUpdateRequest.setPrivilegedMap(privilegeMap);
    bulkProcessUpdateRequest.setUpdatedBy(USERNAME);
    bulkProcessUpdateRequest.setFileName(DUMMY_FILE_NAME);
    bulkProcessUpdateRequest.setFileContent(generateDummyExcelMultipartFile().getBytes());

    gdnBaseRestResponse = new GdnBaseRestResponse();
    gdnBaseRestResponse.setSuccess(Boolean.TRUE);

    fileNames.add(DUMMY_FILE_NAME);
    fileNames.add(ZIP_FILE);

    bulkProcessNotesResponse.setPromoNote(true);
    bulkProcessNotesResponse.setNotes(DEFAULT_ITEM_SKU + COMMA_DELIMITER + DEFAULT_ITEM_NAME);
    bulkProcessNotesResponse.setBulkProcessCode(DEFAULT_PROCESS_CODE);
    pendingRequestsResponse = new BulkPendingRequestsResponse();
    pendingRequestsResponse.setBulkUpdateStatusFlag(true);
    pendingRequestsResponse.setPendingRequestsCount(1);

    bulkDownloadFileContentResponse = new BulkDownloadFileContentResponse();
    bulkDownloadFileContentResponse.setFileContent("test".getBytes());

    this.httpServletResponse = new MockHttpServletResponse();

    unifiedBulkDownloadResponse = new UnifiedBulkDownloadResponse();
    unifiedBulkDownloadResponse.setFilePath("filePath");

    systemParameterConfigResponse = new SystemParameterConfigResponse();
    systemParameterConfigResponse.setUpdatedDate(new Date());
    systemParameterConfigResponse.setVariable(Constants.CATEGORY_BRAND_SCHEDULER_RUN_TIME);

    bulkProcessDeleteOfflineItemRequest = new BulkProcessDeleteOfflineItemRequest();
    bulkProcessDeleteOfflineItemRequest.setBulkProcessType(BulkProcessType.INSTANT_PICKUP_PRODUCT.getValue());
    bulkProcessDeleteOfflineItemRequest.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    bulkProcessDeleteOfflineItemRequest.setFileContent(generateDummyExcelMultipartFile().getBytes());
    bulkProcessDeleteOfflineItemRequest.setFileName(generateDummyExcelMultipartFile().getOriginalFilename());
    bulkProcessDeleteOfflineItemRequest.setUpdatedBy(USERNAME);
    bulkProcessDeleteOfflineItemRequest.setClientHost(CLIENT_HOST);
    bulkProcessDeleteOfflineItemRequest.setPrivilegedMap(Collections.emptyMap());
    Credential.setAccessibilities(new String[] {Accessibilty.FUNCTION_BULK_ARCHIVE_PRODUCTS});
    Mockito.when(gcsProperties.isEnabled()).thenReturn(false);
    ReflectionTestUtils.setField(bulkProcessService, "avoidRedundantDownloadInBulkCreation", false);
  }

  @Test
  public void uploadBulkUpdateTest() throws Exception {
    privilegeMap.put(IS_ONLY_EXTERNAL_USER, true);
    privilegeMap.put(Accessibilty.IS_PRIVILEGED_TO_EDIT_CNC_STATUS, true);
    pendingRequestsResponse.setPendingRequestsCount(0);
    String[] accessibility = {Accessibilty.STORE_PRODUCT_MANAGE_PRODUCT_BULK_UPDATE};
    try (MockedStatic<Credential> mockedCredential = mockStatic(Credential.class)) {
      mockedCredential.when(Credential::getAccessibilities).thenReturn(accessibility);
      Mockito.when(
              businessPartnerService.filterByBusinessPartnerCode(Constants.BUSINESS_PARTNER_CODE))
          .thenReturn(profileResponse);
      Mockito.when(this.xBulkFeign.uploadForBulkUpdate(Mockito.any(BulkProcessV2Request.class)))
          .thenReturn(gdnBaseRestResponse);
      Mockito.when(
              xBulkFeign.checkPendingBulkRequestsByBusinessPartnerCode(Constants.BULK_UPLOAD_TYPE,
                  BUSINESS_PARTNER_CODE, Constants.BULK_PROCESS_TYPE))
          .thenReturn(new GdnRestSingleResponse<>(pendingRequestsResponse, StringUtils.EMPTY));
      this.bulkProcessService.uploadBulkUpdate(BUSINESS_PARTNER_CODE, USERNAME, true,
          generateDummyExcelMultipartFile());
      Mockito.verify(businessPartnerService).filterByBusinessPartnerCode(BUSINESS_PARTNER_CODE);
      Mockito.verify(xBulkFeign).uploadForBulkUpdate(bulkProcessV2RequestArgumentCaptor.capture());
      Mockito.verify(userPicService).fetchAccessiblePickupPointCodes(profileResponse);
      verify(xBulkFeign).checkPendingBulkRequestsByBusinessPartnerCode(Constants.BULK_UPLOAD_TYPE,
          BUSINESS_PARTNER_CODE, Constants.BULK_PROCESS_TYPE);
      assertEquals(BUSINESS_PARTNER_CODE,
          bulkProcessV2RequestArgumentCaptor.getValue().getBusinessPartnerCode());
      assertEquals(BULK_PROCESS_TYPE,
          bulkProcessV2RequestArgumentCaptor.getValue().getBulkProcessType());
      assertEquals(DUMMY_FILE_NAME, bulkProcessV2RequestArgumentCaptor.getValue().getFileName());
      assertEquals(USERNAME, bulkProcessV2RequestArgumentCaptor.getValue().getUpdatedBy());
      assertEquals(privilegeMap, bulkProcessV2RequestArgumentCaptor.getValue().getPrivilegedMap());
      assertNotNull(bulkProcessV2RequestArgumentCaptor.getValue().getFileContent());
    }
  }

  @Test
  public void uploadBulkUpdateForAmphiUserTest() throws Exception {
    privilegeMap.put(IS_ONLY_EXTERNAL_USER, false);
    pendingRequestsResponse.setPendingRequestsCount(0);
    String[] accessibility = {Accessibilty.STORE_PRODUCT_MANAGE_PRODUCT_BULK_UPDATE};
    try (MockedStatic<Credential> mockedCredential = mockStatic(Credential.class)) {
      mockedCredential.when(Credential::getAccessibilities).thenReturn(accessibility);
      Mockito.when(
              businessPartnerService.filterByBusinessPartnerCode(Constants.BUSINESS_PARTNER_CODE))
          .thenReturn(profileResponse);
      Mockito.when(this.xBulkFeign.uploadForBulkUpdate(Mockito.any(BulkProcessV2Request.class)))
          .thenReturn(gdnBaseRestResponse);
      Mockito.when(
              xBulkFeign.checkPendingBulkRequestsByBusinessPartnerCode(Constants.BULK_UPLOAD_TYPE,
                  BUSINESS_PARTNER_CODE, Constants.BULK_PROCESS_TYPE))
          .thenReturn(new GdnRestSingleResponse<>(pendingRequestsResponse, StringUtils.EMPTY));
      this.bulkProcessService.uploadBulkUpdate(BUSINESS_PARTNER_CODE, USERNAME, false,
          generateDummyExcelMultipartFile());
      Mockito.verify(businessPartnerService).filterByBusinessPartnerCode(BUSINESS_PARTNER_CODE);
      Mockito.verify(xBulkFeign).uploadForBulkUpdate(bulkProcessV2RequestArgumentCaptor.capture());
      Mockito.verify(userPicService).fetchAccessiblePickupPointCodes(profileResponse);
      verify(xBulkFeign).checkPendingBulkRequestsByBusinessPartnerCode(Constants.BULK_UPLOAD_TYPE,
          BUSINESS_PARTNER_CODE, Constants.BULK_PROCESS_TYPE);
      assertEquals(BUSINESS_PARTNER_CODE,
          bulkProcessV2RequestArgumentCaptor.getValue().getBusinessPartnerCode());
      assertEquals(BULK_PROCESS_TYPE,
          bulkProcessV2RequestArgumentCaptor.getValue().getBulkProcessType());
      assertEquals(DUMMY_FILE_NAME, bulkProcessV2RequestArgumentCaptor.getValue().getFileName());
      assertEquals(USERNAME, bulkProcessV2RequestArgumentCaptor.getValue().getUpdatedBy());
      assertFalse(bulkProcessV2RequestArgumentCaptor.getValue().getPrivilegedMap()
          .get(IS_ONLY_EXTERNAL_USER));
      assertNotNull(bulkProcessV2RequestArgumentCaptor.getValue().getFileContent());
    }
  }

  @Test
  public void uploadBulkUpdateExceptionTest() throws Exception {
    String[] accessibility = {Accessibilty.STORE_PRODUCT_MANAGE_PRODUCT_BULK_UPDATE};
    pendingRequestsResponse.setPendingRequestsCount(0);
    try (MockedStatic<Credential> mockedCredential = mockStatic(Credential.class)) {
      mockedCredential.when(Credential::getAccessibilities).thenReturn(accessibility);
      gdnBaseRestResponse.setSuccess(Boolean.FALSE);
      Mockito.when(
              businessPartnerService.filterByBusinessPartnerCode(Constants.BUSINESS_PARTNER_CODE))
          .thenReturn(profileResponse);
      Mockito.when(
              xBulkFeign.checkPendingBulkRequestsByBusinessPartnerCode(Constants.BULK_UPLOAD_TYPE,
                  BUSINESS_PARTNER_CODE, Constants.BULK_PROCESS_TYPE))
          .thenReturn(new GdnRestSingleResponse<>(pendingRequestsResponse, StringUtils.EMPTY));
      Mockito.when(this.xBulkFeign.uploadForBulkUpdate(Mockito.any(BulkProcessV2Request.class)))
          .thenReturn(gdnBaseRestResponse);
      try {
        this.bulkProcessService.uploadBulkUpdate(BUSINESS_PARTNER_CODE, USERNAME, true,
            generateDummyExcelMultipartFile());
      } catch (ClientException e) {
        Mockito.verify(businessPartnerService).filterByBusinessPartnerCode(BUSINESS_PARTNER_CODE);
        verify(xBulkFeign).checkPendingBulkRequestsByBusinessPartnerCode(Constants.BULK_UPLOAD_TYPE,
            BUSINESS_PARTNER_CODE, Constants.BULK_PROCESS_TYPE);
        Mockito.verify(xBulkFeign)
            .uploadForBulkUpdate(bulkProcessV2RequestArgumentCaptor.capture());
        Mockito.verify(userPicService).fetchAccessiblePickupPointCodes(profileResponse);
      }
    }
  }

  @Test
  public void uploadBulkUpdateExceptionUnauthorizedErrorTest() throws Exception {
    String errorMessage = "";
    String[] accessibility = {};
    try (MockedStatic<Credential> mockedCredential = mockStatic(Credential.class)) {
      mockedCredential.when(Credential::getAccessibilities).thenReturn(accessibility);
      try {
        this.bulkProcessService.uploadBulkUpdate(BUSINESS_PARTNER_CODE, USERNAME, true,
            generateDummyExcelMultipartFile());
      } catch (ApplicationRuntimeException e) {
        errorMessage = e.getErrorMessage();
      } finally {
        Assertions.assertEquals(UNAUTHORIZE_ERROR, errorMessage);
      }
    }
  }

  @Test
  public void uploadBulkUpdateEANTest() throws Exception {
    pendingRequestsResponse.setPendingRequestsCount(0);
    String[] accessibility = {Accessibilty.STORE_PRODUCT_MANAGE_PRODUCT_BULK_UPDATE};
    try (MockedStatic<Credential> mockedCredential = mockStatic(Credential.class)) {
      mockedCredential.when(Credential::getAccessibilities).thenReturn(accessibility);
      Mockito.when(
              businessPartnerService.filterByBusinessPartnerCode(BUSINESS_PARTNER_CODE))
          .thenReturn(profileResponse);
      Mockito.when(this.xBulkFeign.uploadForBulkUpdateEAN(Mockito.any(BulkProcessV2Request.class)))
          .thenReturn(gdnBaseRestResponse);
      Mockito.when(
              xBulkFeign.checkPendingBulkRequestsByBusinessPartnerCode(Constants.BULK_UPLOAD_TYPE,
                  BUSINESS_PARTNER_CODE, Constants.BULK_PROCESS_TYPE_EAN))
          .thenReturn(new GdnRestSingleResponse<>(pendingRequestsResponse, StringUtils.EMPTY));
      this.bulkProcessService.uploadBulkUpdateEAN(BUSINESS_PARTNER_CODE, USERNAME,
          generateDummyExcelMultipartFile());
      Mockito.verify(businessPartnerService).filterByBusinessPartnerCode(BUSINESS_PARTNER_CODE);
      Mockito.verify(xBulkFeign).uploadForBulkUpdateEAN(bulkProcessV2RequestArgumentCaptor.capture());
      verify(xBulkFeign).checkPendingBulkRequestsByBusinessPartnerCode(Constants.BULK_UPLOAD_TYPE,
          BUSINESS_PARTNER_CODE, Constants.BULK_PROCESS_TYPE_EAN);
      BulkProcessV2Request capturedRequest = bulkProcessV2RequestArgumentCaptor.getValue();
      assertEquals(BUSINESS_PARTNER_CODE, capturedRequest.getBusinessPartnerCode());
      assertEquals(Constants.BULK_PROCESS_TYPE_EAN, capturedRequest.getBulkProcessType());
      assertEquals(DUMMY_FILE_NAME, capturedRequest.getFileName());
      assertEquals(USERNAME, capturedRequest.getUpdatedBy());
      assertNotNull(capturedRequest.getPrivilegedMap());
      assertTrue(capturedRequest.getPrivilegedMap().isEmpty());
      assertNotNull(capturedRequest.getFileContent());
    }
  }

  @Test
  public void uploadBulkUpdateEANExceptionUnauthorizedErrorTest() throws Exception {
    String errorMessage = "";
    String[] accessibility = {};
    try (MockedStatic<Credential> mockedCredential = mockStatic(Credential.class)) {
      mockedCredential.when(Credential::getAccessibilities).thenReturn(accessibility);
      try {
        this.bulkProcessService.uploadBulkUpdateEAN(BUSINESS_PARTNER_CODE, USERNAME,
            generateDummyExcelMultipartFile());
      } catch (ApplicationRuntimeException e) {
        errorMessage = e.getErrorMessage();
      } finally {
        Assertions.assertEquals(UNAUTHORIZE_ERROR, errorMessage);
      }
    }
  }

  @Test
  public void uploadBulkUpdateEANExceptionInvalidBusinessPartnerTest() throws Exception {
    String[] accessibility = {Accessibilty.STORE_PRODUCT_MANAGE_PRODUCT_BULK_UPDATE};
    pendingRequestsResponse.setPendingRequestsCount(0);
    try (MockedStatic<Credential> mockedCredential = mockStatic(Credential.class)) {
      mockedCredential.when(Credential::getAccessibilities).thenReturn(accessibility);
      Mockito.when(
              businessPartnerService.filterByBusinessPartnerCode(BUSINESS_PARTNER_CODE))
          .thenReturn(null);
      Mockito.when(
              xBulkFeign.checkPendingBulkRequestsByBusinessPartnerCode(Constants.BULK_UPLOAD_TYPE,
                  BUSINESS_PARTNER_CODE, Constants.BULK_PROCESS_TYPE_EAN))
          .thenReturn(new GdnRestSingleResponse<>(pendingRequestsResponse, StringUtils.EMPTY));
      try {
        this.bulkProcessService.uploadBulkUpdateEAN(BUSINESS_PARTNER_CODE, USERNAME,
            generateDummyExcelMultipartFile());
      } catch (ApplicationRuntimeException e) {
        Mockito.verify(businessPartnerService).filterByBusinessPartnerCode(BUSINESS_PARTNER_CODE);
        verify(xBulkFeign).checkPendingBulkRequestsByBusinessPartnerCode(Constants.BULK_UPLOAD_TYPE,
            BUSINESS_PARTNER_CODE, Constants.BULK_PROCESS_TYPE_EAN);
        assertTrue(e.getErrorMessage().contains(ErrorMessages.INVALID_BUSINESS_PARTNER_CODE));
      }
    }
  }

  @Test
  public void uploadBulkUpdateEANExceptionInactiveBusinessPartnerTest() throws Exception {
    String[] accessibility = {Accessibilty.STORE_PRODUCT_MANAGE_PRODUCT_BULK_UPDATE};
    pendingRequestsResponse.setPendingRequestsCount(0);
    ProfileResponse inactiveProfileResponse = new ProfileResponse();
    inactiveProfileResponse.setMerchantStatus(INACTIVE_STATUS);
    inactiveProfileResponse.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    try (MockedStatic<Credential> mockedCredential = mockStatic(Credential.class)) {
      mockedCredential.when(Credential::getAccessibilities).thenReturn(accessibility);
      Mockito.when(
              businessPartnerService.filterByBusinessPartnerCode(BUSINESS_PARTNER_CODE))
          .thenReturn(inactiveProfileResponse);
      Mockito.when(
              xBulkFeign.checkPendingBulkRequestsByBusinessPartnerCode(Constants.BULK_UPLOAD_TYPE,
                  BUSINESS_PARTNER_CODE, Constants.BULK_PROCESS_TYPE_EAN))
          .thenReturn(new GdnRestSingleResponse<>(pendingRequestsResponse, StringUtils.EMPTY));
      try {
        this.bulkProcessService.uploadBulkUpdateEAN(BUSINESS_PARTNER_CODE, USERNAME,
            generateDummyExcelMultipartFile());
      } catch (ApplicationRuntimeException e) {
        Mockito.verify(businessPartnerService).filterByBusinessPartnerCode(BUSINESS_PARTNER_CODE);
        verify(xBulkFeign).checkPendingBulkRequestsByBusinessPartnerCode(Constants.BULK_UPLOAD_TYPE,
            BUSINESS_PARTNER_CODE, Constants.BULK_PROCESS_TYPE_EAN);
        assertTrue(e.getErrorMessage().contains(ErrorMessages.INVALID_BUSINESS_PARTNER_CODE));
      }
    }
  }

  @Test
  public void uploadBulkUpdateEANExceptionFailedResponseTest() throws Exception {
    String[] accessibility = {Accessibilty.STORE_PRODUCT_MANAGE_PRODUCT_BULK_UPDATE};
    pendingRequestsResponse.setPendingRequestsCount(0);
    try (MockedStatic<Credential> mockedCredential = mockStatic(Credential.class)) {
      mockedCredential.when(Credential::getAccessibilities).thenReturn(accessibility);
      gdnBaseRestResponse.setSuccess(false);
      Mockito.when(
              businessPartnerService.filterByBusinessPartnerCode(BUSINESS_PARTNER_CODE))
          .thenReturn(profileResponse);
      Mockito.when(
              xBulkFeign.checkPendingBulkRequestsByBusinessPartnerCode(Constants.BULK_UPLOAD_TYPE,
                  BUSINESS_PARTNER_CODE, Constants.BULK_PROCESS_TYPE_EAN))
          .thenReturn(new GdnRestSingleResponse<>(pendingRequestsResponse, StringUtils.EMPTY));
      Mockito.when(this.xBulkFeign.uploadForBulkUpdateEAN(Mockito.any(BulkProcessV2Request.class)))
          .thenReturn(gdnBaseRestResponse);
      try {
        this.bulkProcessService.uploadBulkUpdateEAN(BUSINESS_PARTNER_CODE, USERNAME,
            generateDummyExcelMultipartFile());
      } catch (ClientException e) {
        Mockito.verify(businessPartnerService).filterByBusinessPartnerCode(BUSINESS_PARTNER_CODE);
        verify(xBulkFeign).checkPendingBulkRequestsByBusinessPartnerCode(Constants.BULK_UPLOAD_TYPE,
            BUSINESS_PARTNER_CODE, Constants.BULK_PROCESS_TYPE_EAN);
        Mockito.verify(xBulkFeign)
            .uploadForBulkUpdateEAN(bulkProcessV2RequestArgumentCaptor.capture());
      }
    }
  }

  @Test
  public void uploadEmptyFileExceptionTest() throws Exception {
    fileNames.clear();
    fileNames.add(DUMMY_FILE_NAME);
    File file = generateDummyEmptyExcelFile();
    List<MultipartFile> multipartFiles = new ArrayList<>();
    multipartFiles.add(generateMultipartFile(file));
    Mockito.when(fileStorageService.downloadMultiPartFile(USERNAME,
      fileNames)).thenReturn(multipartFiles);
    pendingRequestsResponse.setPendingRequestsCount(0);
    Mockito.when(businessPartnerService.filterByBusinessPartnerCode(Constants.BUSINESS_PARTNER_CODE))
        .thenReturn(profileResponse);
    Mockito.when(xBulkFeign
        .checkPendingBulkRequestsByBusinessPartnerCode(Constants.BULK_UPLOAD_TYPE, BUSINESS_PARTNER_CODE, Constants.BULK_PRODUCT_CREATION_TYPE))
        .thenReturn(new GdnRestSingleResponse<>(pendingRequestsResponse, StringUtils.EMPTY));
    try {
      this.bulkProcessService.upload(BUSINESS_PARTNER_CODE, USERNAME, fileNames);
    } catch (ApplicationRuntimeException e) {
      assertTrue(e.getErrorMessage().contains(ErrorMessages.EMPTY_FILE));
      verify(businessPartnerService).filterByBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    }
  }

  @Test
  public void uploadOnlyExcelTest() throws Exception {
    fileNames.clear();
    fileNames.add(EXCEL_FILE);
    List<MultipartFile> multipartFiles = new ArrayList<>();
    multipartFiles.add(generateInvalidUpsertXlsm());
    pendingRequestsResponse.setPendingRequestsCount(0);
    Mockito.when(businessPartnerService.filterByBusinessPartnerCode(Constants.BUSINESS_PARTNER_CODE))
        .thenReturn(profileResponse);
    Mockito.when(xBulkFeign.upload(Mockito.any(BulkProcessUploadRequest.class)))
        .thenReturn(new GdnBaseRestResponse(true));
    Mockito.when(xBulkFeign
        .checkPendingBulkRequestsByBusinessPartnerCode(Constants.BULK_UPLOAD_TYPE, BUSINESS_PARTNER_CODE, Constants.BULK_PRODUCT_CREATION_TYPE))
        .thenReturn(new GdnRestSingleResponse<>(pendingRequestsResponse, StringUtils.EMPTY));
    Mockito.when(fileStorageService.downloadMultiPartFile(USERNAME,
      fileNames)).thenReturn(multipartFiles);
    when(userPicService.fetchAccessiblePickupPointCodes(profileResponse)).thenReturn(
        Collections.singleton(PICKUP_POINT_CODE));
    this.bulkProcessService.upload(BUSINESS_PARTNER_CODE, USERNAME, fileNames);
    Mockito.verify(xBulkFeign).upload(bulkProcessUploadRequestArgumentCaptor.capture());
    Mockito.verify(businessPartnerService).filterByBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    verify(xBulkFeign).checkPendingBulkRequestsByBusinessPartnerCode(Constants.BULK_UPLOAD_TYPE, BUSINESS_PARTNER_CODE, Constants.BULK_PRODUCT_CREATION_TYPE);
    assertEquals(BULK_PROCESS_TYPE, bulkProcessUploadRequestArgumentCaptor.getValue().getBulkProcessType());
    assertEquals(BUSINESS_PARTNER_CODE, bulkProcessUploadRequestArgumentCaptor.getValue().getBusinessPartnerCode());
    assertNotNull(bulkProcessUploadRequestArgumentCaptor.getValue().getArgs());
    assertNotNull(bulkProcessUploadRequestArgumentCaptor.getValue().getFiles());
    Mockito.verify(userPicService).fetchAccessiblePickupPointCodes(profileResponse);
    Mockito.verify(mandatoryParameterHelper).isExternalOnly();
  }

  @Test
  public void uploadTest() throws Exception {
    fileNames.clear();
    fileNames.add(EXCEL_FILE);
    fileNames.add(ZIP_FILE);
    pendingRequestsResponse.setPendingRequestsCount(0);
    List<MultipartFile> multipartFiles = new ArrayList<>();
    multipartFiles.add(generateInvalidUpsertXlsm());
    Mockito.when(xBulkFeign.upload(Mockito.any(BulkProcessUploadRequest.class)))
        .thenReturn(new GdnBaseRestResponse(true));
    Mockito.when(businessPartnerService.filterByBusinessPartnerCode(Constants.BUSINESS_PARTNER_CODE))
        .thenReturn(profileResponse);
    Mockito.when(xBulkFeign
        .checkPendingBulkRequestsByBusinessPartnerCode(Constants.BULK_UPLOAD_TYPE, BUSINESS_PARTNER_CODE, Constants.BULK_PRODUCT_CREATION_TYPE))
        .thenReturn(new GdnRestSingleResponse<>(pendingRequestsResponse, StringUtils.EMPTY));
    Mockito.when(fileStorageService.downloadMultiPartFile(USERNAME,
      fileNames)).thenReturn(multipartFiles);
    when(userPicService.fetchAccessiblePickupPointCodes(profileResponse)).thenReturn(
        Collections.singleton(PICKUP_POINT_CODE));
    this.bulkProcessService.upload(BUSINESS_PARTNER_CODE, USERNAME, fileNames);
    Mockito.verify(xBulkFeign).upload(bulkProcessUploadRequestArgumentCaptor.capture());
    Mockito.verify(businessPartnerService).filterByBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    verify(xBulkFeign).checkPendingBulkRequestsByBusinessPartnerCode(Constants.BULK_UPLOAD_TYPE, BUSINESS_PARTNER_CODE, Constants.BULK_PRODUCT_CREATION_TYPE);
    assertEquals(BULK_PROCESS_TYPE, bulkProcessUploadRequestArgumentCaptor.getValue().getBulkProcessType());
    assertEquals(BUSINESS_PARTNER_CODE, bulkProcessUploadRequestArgumentCaptor.getValue().getBusinessPartnerCode());
    assertNotNull(bulkProcessUploadRequestArgumentCaptor.getValue().getArgs());
    assertNotNull(bulkProcessUploadRequestArgumentCaptor.getValue().getFiles());
    assertNotNull(bulkProcessUploadRequestArgumentCaptor.getValue().getFiles());
    Mockito.verify(userPicService).fetchAccessiblePickupPointCodes(profileResponse);
    Mockito.verify(mandatoryParameterHelper).isExternalOnly();
  }

  @Test
  public void uploadWithXlsmFileTest() throws Exception {
    fileNames.clear();
    fileNames.add(EXCEL_FILE_XLSM);
    fileNames.add(ZIP_FILE);
    pendingRequestsResponse.setPendingRequestsCount(0);
    List<MultipartFile> multipartFiles = new ArrayList<>();
    multipartFiles.add(generateDummyExcelMultipartFileXlsm());
    Mockito.when(xBulkFeign.upload(Mockito.any(BulkProcessUploadRequest.class)))
        .thenReturn(new GdnBaseRestResponse(true));
    Mockito.when(businessPartnerService.filterByBusinessPartnerCode(Constants.BUSINESS_PARTNER_CODE))
        .thenReturn(profileResponse);
    Mockito.when(xBulkFeign
        .checkPendingBulkRequestsByBusinessPartnerCode(Constants.BULK_UPLOAD_TYPE, BUSINESS_PARTNER_CODE, Constants.BULK_PRODUCT_CREATION_TYPE))
        .thenReturn(new GdnRestSingleResponse<>(pendingRequestsResponse, StringUtils.EMPTY));
    Mockito.when(fileStorageService.downloadMultiPartFile(USERNAME,
      fileNames)).thenReturn(multipartFiles);
    when(userPicService.fetchAccessiblePickupPointCodes(profileResponse)).thenReturn(
        Collections.singleton(PICKUP_POINT_CODE));
    this.bulkProcessService.upload(BUSINESS_PARTNER_CODE, USERNAME, fileNames);
    Mockito.verify(xBulkFeign).upload(bulkProcessUploadRequestArgumentCaptor.capture());
    Mockito.verify(businessPartnerService).filterByBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    verify(xBulkFeign).checkPendingBulkRequestsByBusinessPartnerCode(Constants.BULK_UPLOAD_TYPE, BUSINESS_PARTNER_CODE, Constants.BULK_PRODUCT_CREATION_TYPE);
    assertEquals(BULK_PROCESS_TYPE, bulkProcessUploadRequestArgumentCaptor.getValue().getBulkProcessType());
    assertEquals(BUSINESS_PARTNER_CODE, bulkProcessUploadRequestArgumentCaptor.getValue().getBusinessPartnerCode());
    assertNotNull(bulkProcessUploadRequestArgumentCaptor.getValue().getArgs());
    assertNotNull(bulkProcessUploadRequestArgumentCaptor.getValue().getFiles());
    assertNotNull(bulkProcessUploadRequestArgumentCaptor.getValue().getFiles());
    Mockito.verify(userPicService).fetchAccessiblePickupPointCodes(profileResponse);
    Mockito.verify(mandatoryParameterHelper).isExternalOnly();
  }

  @Test
  public void uploadWithXlsmFileWithNoBlobReturnedTest() throws Exception {
    fileNames.clear();
    fileNames.add(EXCEL_FILE_XLSM);
    fileNames.add(ZIP_FILE);
    pendingRequestsResponse.setPendingRequestsCount(0);
    List<MultipartFile> multipartFiles = new ArrayList<>();
    Mockito.when(xBulkFeign.upload(Mockito.any(BulkProcessUploadRequest.class)))
      .thenReturn(new GdnBaseRestResponse(true));
    Mockito.when(businessPartnerService.filterByBusinessPartnerCode(Constants.BUSINESS_PARTNER_CODE))
      .thenReturn(profileResponse);
    Mockito.when(xBulkFeign
        .checkPendingBulkRequestsByBusinessPartnerCode(Constants.BULK_UPLOAD_TYPE, BUSINESS_PARTNER_CODE, Constants.BULK_PRODUCT_CREATION_TYPE))
      .thenReturn(new GdnRestSingleResponse<>(pendingRequestsResponse, StringUtils.EMPTY));
    Mockito.when(fileStorageService.downloadMultiPartFile(USERNAME,
      fileNames)).thenReturn(multipartFiles);
    try {
      Assertions.assertThrows(ValidationException.class,
          () -> this.bulkProcessService.upload(BUSINESS_PARTNER_CODE, USERNAME, fileNames));
    }
    finally {
      Mockito.verify(businessPartnerService).filterByBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    }
  }


  @Test
  public void uploadBulkUpdateForBulkArchiveTest() throws Exception {
    try (MockedStatic<Credential> mockedCredential = mockStatic(Credential.class)) {
      mockedCredential.when(Credential::getAccessibilities)
          .thenReturn(new String[] {Accessibilty.FUNCTION_BULK_ARCHIVE_PRODUCTS});
      Mockito.when(this.xBulkFeign.bulkArchiveItemSkus(Mockito.any(BulkProcessUpdateRequest.class)))
          .thenReturn(gdnBaseRestResponse);
      this.bulkProcessService.uploadBulkUpdateForBulkArchive(USERNAME, BUSINESS_PARTNER_CODE,
          generateDummyExcelMultipartFile());
      Mockito.verify(xBulkFeign)
          .bulkArchiveItemSkus(bulkProcessUpdateRequestArgumentCaptor.capture());
      assertEquals(BUSINESS_PARTNER_CODE,
          bulkProcessUpdateRequestArgumentCaptor.getValue().getBusinessPartnerCode());
      assertEquals(BULK_PROCESS_TYPE,
          bulkProcessUpdateRequestArgumentCaptor.getValue().getBulkProcessType());
      assertEquals(DUMMY_FILE_NAME,
          bulkProcessUpdateRequestArgumentCaptor.getValue().getFileName());
      assertEquals(USERNAME, bulkProcessUpdateRequestArgumentCaptor.getValue().getUpdatedBy());
      assertNotNull(bulkProcessUpdateRequestArgumentCaptor.getValue().getFileContent());
    }
  }

  @Test
  void uploadBulkUpdateForBulkArchive_ExceptionTest() throws Exception {
    Credential.setAccessibilities(new String[] {Accessibilty.FUNCTION_MAINTAIN_PRODUK_SKU_SYNC_STOCK});
      Assertions.assertThrows(ApplicationRuntimeException.class,
          () -> this.bulkProcessService.uploadBulkUpdateForBulkArchive(BUSINESS_PARTNER_CODE,
              USERNAME, generateDummyExcelMultipartFile()));
  }

  @Test
  public void bulkUpdateOff2OnTest() throws Exception {
    pendingRequestsResponse.setPendingRequestsCount(0);
    try (MockedStatic<RequestHelper> mockedRequestHelper = mockStatic(RequestHelper.class)) {
      mockedRequestHelper.when(RequestHelper::getEditAccessibilities).thenReturn(privilegeMap);
      mockedRequestHelper.when(
              () -> RequestHelper.checkPrivilegeEditO2O(profileResponse, privilegeMap))
          .thenReturn(Boolean.TRUE);
      this.privilegeMap.put(PRIVILEGED_EDIT_O2O, true);
      Mockito.when(this.businessPartnerService.filterByBusinessPartnerCode(BUSINESS_PARTNER_CODE))
          .thenReturn(profileResponse);
      Mockito.when(this.xBulkFeign.bulkUpdateOff2On(Mockito.any())).thenReturn(gdnBaseRestResponse);
      when(xBulkFeign.checkPendingBulkRequestsByBusinessPartnerCode(Constants.BULK_UPLOAD_TYPE,
          BUSINESS_PARTNER_CODE, Constants.OFF2ON_BULK_PROCESS_TYPE)).thenReturn(
          new GdnRestSingleResponse<>(pendingRequestsResponse, StringUtils.EMPTY));
      this.bulkProcessService.uploadBulkUpdateForInStoreUpdate(USERNAME, BUSINESS_PARTNER_CODE,
          generateDummyExcelMultipartFile());
      Mockito.verify(businessPartnerService).filterByBusinessPartnerCode(BUSINESS_PARTNER_CODE);
      Mockito.verify(xBulkFeign).bulkUpdateOff2On(Mockito.any());
      verify(xBulkFeign).checkPendingBulkRequestsByBusinessPartnerCode(Constants.BULK_UPLOAD_TYPE,
          BUSINESS_PARTNER_CODE, Constants.OFF2ON_BULK_PROCESS_TYPE);
    }
  }

  @Test
  void bulkUpdateOff2OnExceptionTest() throws Exception {
    try (MockedStatic<RequestHelper> mockedRequestHelper = mockStatic(RequestHelper.class)) {
      mockedRequestHelper.when(RequestHelper::getEditAccessibilities).thenReturn(privilegeMap);
      mockedRequestHelper.when(
              () -> RequestHelper.checkPrivilegeEditO2O(profileResponse, privilegeMap))
          .thenReturn(Boolean.FALSE);
      Mockito.when(this.businessPartnerService.filterByBusinessPartnerCode(BUSINESS_PARTNER_CODE))
          .thenReturn(profileResponse);
      Assertions.assertThrows(ApplicationRuntimeException.class,
          () -> this.bulkProcessService.uploadBulkUpdateForInStoreUpdate(USERNAME,
              BUSINESS_PARTNER_CODE, generateDummyExcelMultipartFile()));
    }
    finally {
      verify(this.businessPartnerService).filterByBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    }
  }

  @Test
  void bulkUpdateOff2OnExceptionTestNew() {
    try (MockedStatic<RequestHelper> mockedRequestHelper = mockStatic(RequestHelper.class)) {
      mockedRequestHelper.when(RequestHelper::getEditAccessibilities).thenReturn(privilegeMap);
      mockedRequestHelper.when(
              () -> RequestHelper.checkPrivilegeEditO2O(profileResponse, privilegeMap))
          .thenReturn(Boolean.FALSE);
      Mockito.when(businessPartnerService.filterByBusinessPartnerCode(BUSINESS_PARTNER_CODE))
          .thenReturn(profileResponse);

      Assertions.assertThrows(ApplicationRuntimeException.class,
          () -> bulkProcessService.uploadBulkUpdateForInStoreUpdate(USERNAME, BUSINESS_PARTNER_CODE,
              generateDummyExcelMultipartFile()));
    } finally {
      verify(businessPartnerService).filterByBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    }

  }

  private MultipartFile generateDummyExcelMultipartFile() throws Exception {
    File file = generateDummyExcelFile();
    byte[] fileData = IOUtils.toByteArray(new FileInputStream(file));
    MultipartFile multipartFile = new MockMultipartFile("dummy-excel.xlsx", DUMMY_FILE_NAME, null,
      fileData);
    return multipartFile;
  }


  private MultipartFile generateDummyExcelMultipartFileXlsm() throws Exception {
    File file = generateDummyExcelFile();
    byte[] fileData = IOUtils.toByteArray(new FileInputStream(file));
    MultipartFile multipartFile = new MockMultipartFile("dummy.xlsm", EXCEL_FILE_XLSM, null, fileData);
    return multipartFile;
  }

  private MultipartFile generateDummyExcelMultipartFileUpsertXlsm() throws Exception {
    File file = generateUpsertDummyExcelFile();
    byte[] fileData = IOUtils.toByteArray(new FileInputStream(file));
    MultipartFile multipartFile = new MockMultipartFile("dummyUpsert.xlsx", EXCEL_UPSERT_FILE, null, fileData);
    return multipartFile;
  }

  private MultipartFile generateInvalidUpsertXlsm() throws Exception {
    File file = generateInvalidUpsertDummyExcelFile();
    byte[] fileData = IOUtils.toByteArray(new FileInputStream(file));
    MultipartFile multipartFile = new MockMultipartFile("dummyUpsert.xlsx", EXCEL_UPSERT_FILE, null, fileData);
    return multipartFile;
  }

  private MultipartFile generateDummyExcelMultipartXlsxFile() throws Exception {
    File file = generateDummyExcelFile();
    byte[] fileData = IOUtils.toByteArray(new FileInputStream(file));
    MultipartFile multipartFile = new MockMultipartFile("DummyExcelFile.xlsx", DUMMY_EXCEL_FILE, null,
      fileData);
    return multipartFile;
  }

  private File generateDummyExcelFile() throws Exception {
    ClassLoader classLoader = getClass().getClassLoader();
    File file =
      new File(classLoader.getResource("Productusername" + File.separator + EXCEL_FILE_XLSM).getFile());
    return file;
  }

  private File generateDummyEmptyExcelFile() throws Exception {
    ClassLoader classLoader = getClass().getClassLoader();
    File file =
      new File(classLoader.getResource("Productusername" + File.separator + DUMMY_FILE_NAME).getFile());
    return file;
  }

  private File generateUpsertDummyExcelFile() throws Exception {
    ClassLoader classLoader = getClass().getClassLoader();
    File file =
      new File(classLoader.getResource("Productusername" + File.separator + EXCEL_UPSERT_FILE).getFile());
    return file;
  }

  private File generateInvalidUpsertDummyExcelFile() throws Exception {
    ClassLoader classLoader = getClass().getClassLoader();
    File file =
      new File(classLoader.getResource("Productusername" + File.separator + EXCEL_FILE).getFile());
    return file;
  }

  private MultipartFile generateMultipartFile(File file) throws Exception {
    byte[] fileData = IOUtils.toByteArray(new FileInputStream(file));
    MultipartFile multipartFile = new MockMultipartFile("dummy-excel.xlsx", DUMMY_FILE_NAME, null,
      fileData);
    return multipartFile;
  }


  private void generateDirectoryValue() throws Exception {
    ClassLoader classLoader = getClass().getClassLoader();
    File file =
        new File(classLoader.getResource(
            new StringBuilder().append(FILE_FOLDER).append("/data.txt").toString())
            .getFile());
    Field field = Constants.class.getField("DATA_BASE_DIR");
    Field modifiersField = Field.class.getDeclaredField("modifiers");
    modifiersField.setAccessible(true);
    modifiersField.setInt(field, field.getModifiers() & ~Modifier.FINAL);
    field.setAccessible(true);
    field.set(null, file.getAbsolutePath().replace("/data.txt", ""));
  }

  @Test
  public void uploadTestForXLSX() throws Exception {
    fileNames.clear();
    fileNames.add(DUMMY_EXCEL_FILE);
    fileNames.add(ZIP_FILE);
    List<MultipartFile> multipartFiles = new ArrayList<>();
    multipartFiles.add(generateDummyExcelMultipartXlsxFile());
    pendingRequestsResponse.setPendingRequestsCount(0);
    Mockito.when(xBulkFeign.upload(Mockito.any(BulkProcessUploadRequest.class)))
        .thenReturn(new GdnBaseRestResponse(true));
    Mockito.when(businessPartnerService.filterByBusinessPartnerCode(Constants.BUSINESS_PARTNER_CODE))
        .thenReturn(profileResponse);
    Mockito.when(fileStorageService.downloadMultiPartFile(USERNAME,
      fileNames)).thenReturn(multipartFiles);
    Mockito.when(xBulkFeign
        .checkPendingBulkRequestsByBusinessPartnerCode(Constants.BULK_UPLOAD_TYPE, BUSINESS_PARTNER_CODE, Constants.BULK_PRODUCT_CREATION_TYPE))
        .thenReturn(new GdnRestSingleResponse<>(pendingRequestsResponse, StringUtils.EMPTY));
    when(userPicService.fetchAccessiblePickupPointCodes(profileResponse)).thenReturn(
        Collections.singleton(PICKUP_POINT_CODE));
    this.bulkProcessService.upload(BUSINESS_PARTNER_CODE, USERNAME, fileNames);
    Mockito.verify(xBulkFeign).upload(bulkProcessUploadRequestArgumentCaptor.capture());
    Mockito.verify(businessPartnerService).filterByBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    assertEquals(BULK_PROCESS_TYPE, bulkProcessUploadRequestArgumentCaptor.getValue().getBulkProcessType());
    verify(xBulkFeign).checkPendingBulkRequestsByBusinessPartnerCode(Constants.BULK_UPLOAD_TYPE, BUSINESS_PARTNER_CODE, Constants.BULK_PRODUCT_CREATION_TYPE);
    assertEquals(BUSINESS_PARTNER_CODE, bulkProcessUploadRequestArgumentCaptor.getValue().getBusinessPartnerCode());
    assertNotNull(bulkProcessUploadRequestArgumentCaptor.getValue().getArgs());
    assertNotNull(bulkProcessUploadRequestArgumentCaptor.getValue().getFiles());
    assertNotNull(bulkProcessUploadRequestArgumentCaptor.getValue().getFiles());
    Mockito.verify(userPicService).fetchAccessiblePickupPointCodes(profileResponse);
    Mockito.verify(mandatoryParameterHelper).isExternalOnly();
  }

  @Test
  public void filterPromoBulkProcessNotesByBulkProcessCodeTest() {
    Mockito.when(this.xBulkFeign.filterPromoBulkProcessNotesByBulkProcessCode(DEFAULT_PROCESS_CODE))
        .thenReturn(new GdnRestListResponse<>(null, null, true, Arrays.asList(bulkProcessNotesResponse), null, null));
    List<PromoUpdateProductResponse> responses = bulkProcessService.fetchPromoUpdatedProductNotes(DEFAULT_PROCESS_CODE);
    Mockito.verify(this.xBulkFeign).filterPromoBulkProcessNotesByBulkProcessCode(DEFAULT_PROCESS_CODE);
    Assertions.assertEquals(responses.get(0).getItemSku(), DEFAULT_ITEM_SKU);
    Assertions.assertEquals(responses.get(0).getItemName(), DEFAULT_ITEM_NAME);
  }

  @Test
  public void fetchWholeSaleConfigCountTest() {
    Mockito.when(this.xBulkFeign.filterWholeSaleConfigBulkProcessNotesByBulkProcessCode(DEFAULT_PROCESS_CODE))
        .thenReturn(
            new GdnRestSingleResponse<>(null, null, true, new WholeSaleCountResponse(10, 10, 10, "path"), null));
    WholesaleCountWebResponse response = bulkProcessService.fetchWholeSaleConfigCount(DEFAULT_PROCESS_CODE);
    Mockito.verify(this.xBulkFeign).filterWholeSaleConfigBulkProcessNotesByBulkProcessCode(DEFAULT_PROCESS_CODE);
    Assertions.assertEquals("path", response.getDownloadFilePath());
    Assertions.assertEquals(10, response.getWholeSaleTurnOffCount());
    Assertions.assertEquals(10, response.getWholeSaleFailedCount());
    Assertions.assertEquals(10, response.getWholeSaleUpdatedCount());
  }

  @Test
  public void checkForPendingBulkProcessByBusinessPartnerCode() {
    Mockito.when(this.xBulkFeign.checkPendingBulkRequestsByBusinessPartnerCode(Constants.BULK_UPLOAD_TYPE, BUSINESS_PARTNER_CODE, BULK_PROCESS_TYPE))
        .thenReturn(new GdnRestSingleResponse<>(pendingRequestsResponse, StringUtils.EMPTY));
    BulkPendingRequestsWebResponse bulkPendingRequestsWebResponse =
        bulkProcessService.checkPendingBulkProcess(Constants.BULK_UPLOAD_TYPE, BUSINESS_PARTNER_CODE, BULK_PROCESS_TYPE);
    Mockito.verify(this.xBulkFeign).checkPendingBulkRequestsByBusinessPartnerCode(Constants.BULK_UPLOAD_TYPE, BUSINESS_PARTNER_CODE, BULK_PROCESS_TYPE);
    Assertions.assertEquals(bulkPendingRequestsWebResponse.getPendingRequestsCount(), 1);

  }

  @Test
  public void downloadProductUnifiedTemplate_restrictAccessTest() throws Exception {
    PickupPointCodesRequestDTO request =
      new PickupPointCodesRequestDTO(Collections.singleton(PICKUP_POINT_CODE));
    when(userPicService.shouldRestrictAccess(null)).thenReturn(true);
    when(mandatoryParameterHelper.getPickupPoints()).thenReturn(request.getPickupPointCodes());
    when(this.xBulkFeign.downloadProductUnifiedTemplateV2(BUSINESS_PARTNER_CODE, request))
      .thenReturn(new GdnRestSimpleResponse<>(StringUtils.EMPTY, unifiedBulkDownloadResponse));
    this.bulkProcessService.downloadProductUnifiedTemplate(BUSINESS_PARTNER_CODE);
    verify(userPicService).shouldRestrictAccess(null);
    verify(mandatoryParameterHelper).getPickupPoints();
    verify(this.xBulkFeign).downloadProductUnifiedTemplateV2(BUSINESS_PARTNER_CODE, request);
    Assertions.assertEquals(bulkDownloadFileContentResponse.getFileContent().length, 4);
  }

  @Test
  public void downloadProductUnifiedTemplateTest() throws Exception {
    when(userPicService.shouldRestrictAccess(null)).thenReturn(false);
    when(this.xBulkFeign.downloadProductUnifiedTemplate(BUSINESS_PARTNER_CODE))
      .thenReturn(new GdnRestSimpleResponse<>(StringUtils.EMPTY, unifiedBulkDownloadResponse));
    this.bulkProcessService.downloadProductUnifiedTemplate(BUSINESS_PARTNER_CODE);
    verify(userPicService).shouldRestrictAccess(null);
    Mockito.verify(this.xBulkFeign).downloadProductUnifiedTemplate(BUSINESS_PARTNER_CODE);
    Assertions.assertEquals(bulkDownloadFileContentResponse.getFileContent().length, 4);
  }

  @Test
  public void getBulkSystemParameterConfigTest() {
    Mockito.when(this.xBulkFeign.findOne(Constants.CATEGORY_BRAND_SCHEDULER_RUN_TIME))
        .thenReturn(new GdnRestSingleResponse<>(systemParameterConfigResponse, StringUtils.EMPTY));
    this.bulkProcessService.getBulkSystemParameterConfig(Constants.CATEGORY_BRAND_SCHEDULER_RUN_TIME);
    Mockito.verify(this.xBulkFeign).findOne(Constants.CATEGORY_BRAND_SCHEDULER_RUN_TIME);
    assertNotNull(systemParameterConfigResponse);
    Assertions.assertEquals(systemParameterConfigResponse.getVariable(), Constants.CATEGORY_BRAND_SCHEDULER_RUN_TIME);
  }

  @Test
  public void uploadBulkUpdateForBulkArchiveProductSkusTest() throws Exception {
     pendingRequestsResponse.setPendingRequestsCount(0);
    try (MockedStatic<Credential> mockedCredential = mockStatic(Credential.class)) {
      mockedCredential.when(Credential::getAccessibilities)
          .thenReturn(new String[] {Accessibilty.FUNCTION_BULK_ARCHIVE_PRODUCTS});
      Mockito.when(
              this.xBulkFeign.bulkArchiveProductSkus(Mockito.any(BulkProcessUpdateRequest.class)))
          .thenReturn(gdnBaseRestResponse);
      when(xBulkFeign.checkPendingBulkRequestsByBusinessPartnerCode(Constants.BULK_UPLOAD_TYPE,
          BUSINESS_PARTNER_CODE, Constants.ARCHIVE_BULK_PROCESS_TYPE)).thenReturn(
          new GdnRestSingleResponse<>(pendingRequestsResponse, StringUtils.EMPTY));
      this.bulkProcessService.uploadBulkUpdateForBulkArchiveProductSkus(USERNAME,
          BUSINESS_PARTNER_CODE, generateDummyExcelMultipartFile());
      Mockito.verify(xBulkFeign)
          .bulkArchiveProductSkus(bulkProcessUpdateRequestArgumentCaptor.capture());
      verify(xBulkFeign).checkPendingBulkRequestsByBusinessPartnerCode(Constants.BULK_UPLOAD_TYPE,
          BUSINESS_PARTNER_CODE, Constants.ARCHIVE_BULK_PROCESS_TYPE);
      assertEquals(BUSINESS_PARTNER_CODE,
          bulkProcessUpdateRequestArgumentCaptor.getValue().getBusinessPartnerCode());
      assertEquals(BULK_PROCESS_TYPE,
          bulkProcessUpdateRequestArgumentCaptor.getValue().getBulkProcessType());
      assertEquals(DUMMY_FILE_NAME,
          bulkProcessUpdateRequestArgumentCaptor.getValue().getFileName());
      assertEquals(USERNAME, bulkProcessUpdateRequestArgumentCaptor.getValue().getUpdatedBy());
      assertNotNull(bulkProcessUpdateRequestArgumentCaptor.getValue().getFileContent());
    }
  }

  @Test
  void uploadBulkUpdateForBulkArchiveProductSkus_PendingTest() throws Exception {
    try (MockedStatic<Credential> mockedCredential = mockStatic(Credential.class)) {
      mockedCredential.when(Credential::getAccessibilities)
          .thenReturn(new String[] {Accessibilty.FUNCTION_BULK_ARCHIVE_PRODUCTS});
      when(xBulkFeign.checkPendingBulkRequestsByBusinessPartnerCode(Constants.BULK_UPLOAD_TYPE,
          BUSINESS_PARTNER_CODE, Constants.ARCHIVE_BULK_PROCESS_TYPE)).thenReturn(
          new GdnRestSingleResponse<>(pendingRequestsResponse, StringUtils.EMPTY));
      try {
        Assertions.assertThrows(ApplicationRuntimeException.class,
            () -> this.bulkProcessService.uploadBulkUpdateForBulkArchiveProductSkus(USERNAME,
                BUSINESS_PARTNER_CODE, generateDummyExcelMultipartFile()));
      } finally {
        verify(xBulkFeign).checkPendingBulkRequestsByBusinessPartnerCode(Constants.BULK_UPLOAD_TYPE,
            BUSINESS_PARTNER_CODE, Constants.ARCHIVE_BULK_PROCESS_TYPE);
      }
    }
  }

  @Test
  public void uploadBulkUpdateForBulkArchiveProductSku_ExceptionTest() throws Exception {
    Credential.setAccessibilities(new String[] {Accessibilty.FUNCTION_MAINTAIN_PRODUK_SKU_SYNC_STOCK});
      Assertions.assertThrows(ApplicationRuntimeException.class,
          () -> this.bulkProcessService.uploadBulkUpdateForBulkArchiveProductSkus(
              BUSINESS_PARTNER_CODE, USERNAME, generateDummyExcelMultipartFile()));
  }

  @Test
  public void uploadBulkSubjectToVatSkusTest() throws Exception {
    pendingRequestsResponse.setPendingRequestsCount(0);
    Mockito.when(this.xBulkFeign.bulkUploadSubjectToVat(Mockito.any(BulkProcessSubjectToVatRequest.class)))
        .thenReturn(gdnBaseRestResponse);
    when(xBulkFeign
        .checkPendingBulkRequestsByBusinessPartnerCode(Constants.BULK_UPLOAD_TYPE, BUSINESS_PARTNER_CODE, Constants.SUBJECT_TO_VAT_TYPE))
        .thenReturn(new GdnRestSingleResponse<>(pendingRequestsResponse, StringUtils.EMPTY));
    this.bulkProcessService
        .uploadBulkSubjectToVatSkus(BUSINESS_PARTNER_CODE, Constants.SUBJECT_TO_VAT_FOLDER, BULK_PROCESS_CODE);
    Mockito.verify(xBulkFeign).bulkUploadSubjectToVat(bulkProcessSubjectToVatRequestArgumentCaptor.capture());
    verify(xBulkFeign)
        .checkPendingBulkRequestsByBusinessPartnerCode(Constants.BULK_UPLOAD_TYPE, BUSINESS_PARTNER_CODE, Constants.SUBJECT_TO_VAT_TYPE);
    assertEquals(BUSINESS_PARTNER_CODE, bulkProcessSubjectToVatRequestArgumentCaptor.getValue().getBusinessPartnerCode());
    assertEquals(Constants.SUBJECT_TO_VAT_FOLDER, bulkProcessSubjectToVatRequestArgumentCaptor.getValue().getFileName());
  }

  @Test
  void uploadBulkSubjectToVatSkus_PendingTest() throws Exception {
    when(xBulkFeign
        .checkPendingBulkRequestsByBusinessPartnerCode(Constants.BULK_UPLOAD_TYPE, BUSINESS_PARTNER_CODE, Constants.SUBJECT_TO_VAT_TYPE))
        .thenReturn(new GdnRestSingleResponse<>(pendingRequestsResponse, StringUtils.EMPTY));
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class,
          () -> this.bulkProcessService.uploadBulkSubjectToVatSkus(BUSINESS_PARTNER_CODE,
              Constants.SUBJECT_TO_VAT_FOLDER, BULK_PROCESS_CODE));
    } finally {
      verify(xBulkFeign)
          .checkPendingBulkRequestsByBusinessPartnerCode(Constants.BULK_UPLOAD_TYPE, BUSINESS_PARTNER_CODE, Constants.SUBJECT_TO_VAT_TYPE);
    }
  }

  @Test
  void uploadBulkSubjectToVatSkus_ExceptionTest() throws Exception {
    try {
      pendingRequestsResponse.setPendingRequestsCount(0);
      when(xBulkFeign
          .checkPendingBulkRequestsByBusinessPartnerCode(Constants.BULK_UPLOAD_TYPE, BUSINESS_PARTNER_CODE, Constants.SUBJECT_TO_VAT_TYPE))
          .thenReturn(new GdnRestSingleResponse<>(pendingRequestsResponse, StringUtils.EMPTY));
      when(xBulkFeign.bulkUploadSubjectToVat(Mockito.any(BulkProcessSubjectToVatRequest.class)))
          .thenThrow(ApplicationRuntimeException.class);
      Assertions.assertThrows(ApplicationRuntimeException.class,
          () -> this.bulkProcessService.uploadBulkSubjectToVatSkus(BUSINESS_PARTNER_CODE,
              Constants.SUBJECT_TO_VAT_FOLDER, BULK_PROCESS_CODE));
    } finally {
      verify(xBulkFeign)
          .checkPendingBulkRequestsByBusinessPartnerCode(Constants.BULK_UPLOAD_TYPE, BUSINESS_PARTNER_CODE, Constants.SUBJECT_TO_VAT_TYPE);
      Mockito.verify(xBulkFeign).bulkUploadSubjectToVat(bulkProcessSubjectToVatRequestArgumentCaptor.capture());
    }
  }

  @Test
  public void uploadBulkDeleteOfflineItemsTest() throws Exception {
    Mockito.when(userPicService.fetchAccessiblePickupPointCodes(null)).thenReturn(new HashSet<>());
    try (MockedStatic<RequestHelper> mockedRequestHelper = mockStatic(RequestHelper.class)) {
      mockedRequestHelper.when(
              () -> RequestHelper.toBulkProcessDeleteOfflineItemRequest(BUSINESS_PARTNER_CODE,
                  USERNAME,
                  CLIENT_HOST, generateDummyExcelMultipartFile(), new HashSet<>()))
          .thenReturn(bulkProcessDeleteOfflineItemRequest);
      when(xBulkFeign.bulkUploadDeleteOfflineItems(Mockito.any())).thenReturn(gdnBaseRestResponse);
      bulkProcessService.uploadBulkDeleteOfflineItems(REQUEST_ID, BUSINESS_PARTNER_CODE, USERNAME,
          CLIENT_HOST, generateDummyExcelMultipartFile());
      Mockito.verify(xBulkFeign).bulkUploadDeleteOfflineItems(
          bulkProcessDeleteOfflineItemRequestArgumentCaptor.capture());
      Mockito.verify(userPicService).fetchAccessiblePickupPointCodes(null);
    }
  }

  @Test
  public void uploadBulkDeleteOfflineItems_withAccessiblePickupPointsTest() throws Exception {
    when(xBulkFeign.bulkUploadDeleteOfflineItems(Mockito.any())).thenReturn(gdnBaseRestResponse);
      when(userPicService.fetchAccessiblePickupPointCodes(null)).thenReturn(
        Collections.singleton(PICKUP_POINT_CODE));
    bulkProcessService
      .uploadBulkDeleteOfflineItems(REQUEST_ID, BUSINESS_PARTNER_CODE, USERNAME,
        CLIENT_HOST, generateDummyExcelMultipartFile());
    Mockito.verify(xBulkFeign)
      .bulkUploadDeleteOfflineItems(bulkProcessDeleteOfflineItemRequestArgumentCaptor.capture());
    Mockito.verify(userPicService).fetchAccessiblePickupPointCodes(null);
    Assertions.assertEquals(PICKUP_POINT_CODE,
      bulkProcessDeleteOfflineItemRequestArgumentCaptor.getValue().getAccessiblePickupPoints()
        .stream().findFirst().get());
  }

  @AfterEach
  public void teardown() throws Exception {
    verifyNoMoreInteractions(this.xBulkFeign);
    verifyNoMoreInteractions(this.businessPartnerService);
    verifyNoMoreInteractions(userPicService);
    verifyNoMoreInteractions(mandatoryParameterHelper);
  }

  @Test
  public void uploadBulkUpsertTest() throws Exception {
    ReflectionTestUtils.setField(bulkProcessService, "multiPickupPointEnabled", true);
    fileNames.clear();
    fileNames.add(EXCEL_UPSERT_FILE);
    List<MultipartFile> multipartFiles = new ArrayList<>();
    multipartFiles.add(generateDummyExcelMultipartFileUpsertXlsm());
    pendingRequestsResponse.setPendingRequestsCount(0);
    Mockito.when(businessPartnerService.filterByBusinessPartnerCode(Constants.BUSINESS_PARTNER_CODE))
        .thenReturn(profileResponse);
    Mockito.when(fileStorageService.downloadMultiPartFile(USERNAME,
      fileNames)).thenReturn(multipartFiles);
    Mockito.when(xBulkFeign.uploadForBulkUpsertOfflineItems(Mockito.any()))
        .thenReturn(new GdnBaseRestResponse(true));
    Mockito.when(xBulkFeign.checkPendingBulkRequestsByBusinessPartnerCode(Constants.BULK_UPLOAD_TYPE, BUSINESS_PARTNER_CODE,
            BulkProcessType.INSTANT_PICKUP_PRODUCT_UPSERT.getValue()))
        .thenReturn(new GdnRestSingleResponse<>(pendingRequestsResponse, StringUtils.EMPTY));
    this.bulkProcessService.upload(BUSINESS_PARTNER_CODE, USERNAME, fileNames);
    Mockito.verify(xBulkFeign).uploadForBulkUpsertOfflineItems(bulkProcessUpsertOfflineItemRequestArgumentCaptor.capture());
    Mockito.verify(businessPartnerService).filterByBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    verify(xBulkFeign).checkPendingBulkRequestsByBusinessPartnerCode(Constants.BULK_UPLOAD_TYPE, BUSINESS_PARTNER_CODE,
        BulkProcessType.INSTANT_PICKUP_PRODUCT_UPSERT.getValue());
    assertEquals(BulkProcessType.INSTANT_PICKUP_PRODUCT.getValue(), bulkProcessUpsertOfflineItemRequestArgumentCaptor.getValue().getBulkProcessType());
    assertEquals(BUSINESS_PARTNER_CODE, bulkProcessUpsertOfflineItemRequestArgumentCaptor.getValue().getBusinessPartnerCode());
    verify(userPicService).fetchAccessiblePickupPointCodes(profileResponse);
    assertEquals(BulkProcessType.INSTANT_PICKUP_PRODUCT.getValue(),
      bulkProcessUpsertOfflineItemRequestArgumentCaptor.getValue().getBulkProcessType());
    assertEquals(BUSINESS_PARTNER_CODE,
      bulkProcessUpsertOfflineItemRequestArgumentCaptor.getValue().getBusinessPartnerCode());
  }

  @Test
  public void uploadBulkUpsertWithInvalidFileTest() throws Exception {
    ReflectionTestUtils.setField(bulkProcessService, "multiPickupPointEnabled", true);
    fileNames.clear();
    fileNames.add(EXCEL_FILE);
    List<MultipartFile> multipartFiles = new ArrayList<>();
    multipartFiles.add(generateInvalidUpsertXlsm());
    pendingRequestsResponse.setPendingRequestsCount(0);
    Mockito.when(businessPartnerService.filterByBusinessPartnerCode(Constants.BUSINESS_PARTNER_CODE))
        .thenReturn(profileResponse);
    Mockito.when(xBulkFeign.upload(Mockito.any()))
        .thenReturn(new GdnBaseRestResponse(true));
    Mockito.when(xBulkFeign.checkPendingBulkRequestsByBusinessPartnerCode(Constants.BULK_UPLOAD_TYPE, BUSINESS_PARTNER_CODE,
            Constants.BULK_PRODUCT_CREATION_TYPE))
        .thenReturn(new GdnRestSingleResponse<>(pendingRequestsResponse, StringUtils.EMPTY));
    Mockito.when(fileStorageService.downloadMultiPartFile(USERNAME,
      fileNames)).thenReturn(multipartFiles);
    when(userPicService.fetchAccessiblePickupPointCodes(profileResponse)).thenReturn(
        Collections.singleton(PICKUP_POINT_CODE));
    this.bulkProcessService.upload(BUSINESS_PARTNER_CODE, USERNAME, fileNames);
    Mockito.verify(xBulkFeign).upload(bulkProcessUploadRequestArgumentCaptor.capture());
    Mockito.verify(businessPartnerService).filterByBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    verify(xBulkFeign).checkPendingBulkRequestsByBusinessPartnerCode(Constants.BULK_UPLOAD_TYPE, BUSINESS_PARTNER_CODE,
        Constants.BULK_PRODUCT_CREATION_TYPE);
    assertEquals(BULK_PROCESS_TYPE, bulkProcessUploadRequestArgumentCaptor.getValue().getBulkProcessType());
    assertEquals(BUSINESS_PARTNER_CODE, bulkProcessUploadRequestArgumentCaptor.getValue().getBusinessPartnerCode());
    assertNotNull(bulkProcessUploadRequestArgumentCaptor.getValue().getArgs());
    assertNotNull(bulkProcessUploadRequestArgumentCaptor.getValue().getFiles());
    Mockito.verify(userPicService).fetchAccessiblePickupPointCodes(profileResponse);
    Mockito.verify(mandatoryParameterHelper).isExternalOnly();
  }

  @Test
  public void fetchBulkProcessListingWebResponse_ValidInput_ReturnsPage() throws Exception {
    ReflectionTestUtils.setField(bulkProcessService, "bulkProcessListingEnabled", true);
    List<BulkProcessStatusListingWebResponse> expectedListingWebResponses = new ArrayList<>();

    GdnRestListResponse<BulkProcessStatusListingResponse> listingResponse = new GdnRestListResponse<>();
    listingResponse.setContent(Collections.singletonList(
      BulkProcessStatusListingResponse.builder().bulkProcessCode(BULK_PROCESS_CODE)
        .isProcessCompleted(true).bulkProcessType(BULK_PROCESS_TYPE).build()));
    listingResponse.setPageMetaData(new PageMetaData(0,1,1));
    listingResponse.setSuccess(true);
    Mockito.when(xBulkFeign.fetchBulkProcessStatusListing(Constants.STORE_ID, Constants.CHANNEL_ID,
        Constants.CLIENT_ID, REQUEST_ID, BULK_PROCESS_TYPE, BUSINESS_PARTNER_CODE,
        new ArrayList(), false,0, 1))
      .thenReturn(listingResponse);
    Page<BulkProcessStatusListingWebResponse> result =
      bulkProcessService.fetchBulkProcessListingWebResponse(
      Constants.STORE_ID, REQUEST_ID, BUSINESS_PARTNER_CODE, BULK_PROCESS_TYPE, Optional.empty(),
        false, 0, 1);
    Mockito.verify(xBulkFeign).fetchBulkProcessStatusListing(Constants.STORE_ID, Constants.CHANNEL_ID,
      Constants.CLIENT_ID, REQUEST_ID, BULK_PROCESS_TYPE, BUSINESS_PARTNER_CODE,new ArrayList(),
      false,
      0, 1);
    assertNotNull(result);
    Assertions.assertTrue(CollectionUtils.isNotEmpty(result.getContent()));
  }

  @Test
  public void fetchBulkProcessListingWebResponseWithProcessCodes() throws Exception {
    ReflectionTestUtils.setField(bulkProcessService, "bulkProcessListingEnabled", true);
    List<BulkProcessStatusListingWebResponse> expectedListingWebResponses = new ArrayList<>();

    GdnRestListResponse<BulkProcessStatusListingResponse> listingResponse = new GdnRestListResponse<>();
    listingResponse.setContent(Collections.singletonList(
      BulkProcessStatusListingResponse.builder().bulkProcessCode(BULK_PROCESS_CODE)
        .isProcessCompleted(true).bulkProcessType(BULK_PROCESS_TYPE).build()));
    listingResponse.setPageMetaData(new PageMetaData(0,1,1));
    listingResponse.setSuccess(true);
    Mockito.when(xBulkFeign.fetchBulkProcessStatusListing(Constants.STORE_ID, Constants.CHANNEL_ID,
        Constants.CLIENT_ID, REQUEST_ID, BULK_PROCESS_TYPE, BUSINESS_PARTNER_CODE,
        Collections.singletonList(BULK_PROCESS_CODE),false, 0, 1))
      .thenReturn(listingResponse);
    Page<BulkProcessStatusListingWebResponse> result =
      bulkProcessService.fetchBulkProcessListingWebResponse(
        Constants.STORE_ID, REQUEST_ID, BUSINESS_PARTNER_CODE, BULK_PROCESS_TYPE,
        Optional.of(Collections.singletonList(BULK_PROCESS_CODE)), false, 0, 1);
    Mockito.verify(xBulkFeign).fetchBulkProcessStatusListing(Constants.STORE_ID, Constants.CHANNEL_ID,
      Constants.CLIENT_ID, REQUEST_ID, BULK_PROCESS_TYPE, BUSINESS_PARTNER_CODE,
      Collections.singletonList(BULK_PROCESS_CODE),false,
      0, 1);
    assertNotNull(result);
    Assertions.assertTrue(CollectionUtils.isNotEmpty(result.getContent()));
  }

  @Test
  void fetchBulkProcessListingWebResponse_ValidInput_exception() throws Exception {
    ReflectionTestUtils.setField(bulkProcessService, "bulkProcessListingEnabled", true);
    List<BulkProcessStatusListingWebResponse> expectedListingWebResponses = new ArrayList<>();

    GdnRestListResponse<BulkProcessStatusListingResponse> listingResponse = new GdnRestListResponse<>();
    listingResponse.setContent(Collections.singletonList(
      BulkProcessStatusListingResponse.builder().bulkProcessCode(BULK_PROCESS_CODE)
        .isProcessCompleted(true).bulkProcessType(BULK_PROCESS_TYPE).build()));
    listingResponse.setPageMetaData(new PageMetaData(0,1,1));
    listingResponse.setSuccess(false);
    Mockito.when(xBulkFeign.fetchBulkProcessStatusListing(Constants.STORE_ID, Constants.CHANNEL_ID,
        Constants.CLIENT_ID, REQUEST_ID, BULK_PROCESS_TYPE, BUSINESS_PARTNER_CODE,
        new ArrayList(), false,0, 1))
      .thenReturn(listingResponse);
    try {
      Assertions.assertThrows(ClientException.class,
          () -> bulkProcessService.fetchBulkProcessListingWebResponse(Constants.STORE_ID,
              REQUEST_ID, BUSINESS_PARTNER_CODE, BULK_PROCESS_TYPE, Optional.empty(), false, 0, 1));
    }
    finally {
      Mockito.verify(xBulkFeign).fetchBulkProcessStatusListing(Constants.STORE_ID, Constants.CHANNEL_ID,
        Constants.CLIENT_ID, REQUEST_ID, BULK_PROCESS_TYPE, BUSINESS_PARTNER_CODE,
        new ArrayList(), false,0, 1);
    }
  }

@Test
public void fetchBulkProcessListingWebResponse_NullInput_exception() throws Exception {
  ReflectionTestUtils.setField(bulkProcessService, "bulkProcessListingEnabled", true);
  List<BulkProcessStatusListingWebResponse> expectedListingWebResponses = new ArrayList<>();

    GdnRestListResponse<BulkProcessStatusListingResponse> listingResponse = new GdnRestListResponse<>();
    listingResponse.setContent(null);
    listingResponse.setPageMetaData(new PageMetaData(0,1,1));
    Mockito.when(xBulkFeign.fetchBulkProcessStatusListing(Constants.STORE_ID, Constants.CHANNEL_ID,
        Constants.CLIENT_ID, REQUEST_ID, BULK_PROCESS_TYPE, BUSINESS_PARTNER_CODE,
        new ArrayList(), false,0, 1))
      .thenReturn(listingResponse);
    try {
      Page<BulkProcessStatusListingWebResponse> result =
        bulkProcessService.fetchBulkProcessListingWebResponse(Constants.STORE_ID, REQUEST_ID,
          BUSINESS_PARTNER_CODE, BULK_PROCESS_TYPE, Optional.empty(), false, 0, 1);
    }
    finally {
      Mockito.verify(xBulkFeign).fetchBulkProcessStatusListing(Constants.STORE_ID, Constants.CHANNEL_ID,
        Constants.CLIENT_ID, REQUEST_ID, BULK_PROCESS_TYPE, BUSINESS_PARTNER_CODE,
        new ArrayList(),false,0, 1);
    }
  }

  @Test
  public void fetchBulkProcessListingWebResponseWithSwitchOff() throws Exception {
    ReflectionTestUtils.setField(bulkProcessService, "bulkProcessListingEnabled", false);
    List<BulkProcessStatusListingWebResponse> expectedListingWebResponses = new ArrayList<>();

    GdnRestListResponse<BulkProcessStatusListingResponse> listingResponse = new GdnRestListResponse<>();
    listingResponse.setContent(Collections.singletonList(
      BulkProcessStatusListingResponse.builder().bulkProcessCode(BULK_PROCESS_CODE)
        .isProcessCompleted(true).bulkProcessType(BULK_PROCESS_TYPE).build()));
    listingResponse.setPageMetaData(new PageMetaData(0,1,1));
    listingResponse.setSuccess(true);
    Mockito.when(xBulkFeign.fetchBulkProcessStatusListing(Constants.STORE_ID, Constants.CHANNEL_ID,
        Constants.CLIENT_ID, REQUEST_ID, BULK_PROCESS_TYPE, BUSINESS_PARTNER_CODE,
        Collections.singletonList(BULK_PROCESS_CODE),false, 0, 1))
      .thenReturn(listingResponse);
    Page<BulkProcessStatusListingWebResponse> result =
      bulkProcessService.fetchBulkProcessListingWebResponse(
        Constants.STORE_ID, REQUEST_ID, BUSINESS_PARTNER_CODE, BULK_PROCESS_TYPE,
        Optional.of(Collections.singletonList(BULK_PROCESS_CODE)), false, 0, 1);
    Assertions.assertTrue(CollectionUtils.isEmpty(result.getContent()));
  }

  @Test
  public void uploadBulkForWorkOrderCreationTest() throws Exception {
    ReflectionTestUtils.setField(bulkProcessService, "productBundlingEnabled", true);
    ReflectionTestUtils.setField(bulkProcessService, "bundlingAllowedSellerType", "TD");
    ProfileResponse profileResponse = new ProfileResponse();
    CompanyDTO companyDTO = new CompanyDTO();
    companyDTO.setMerchantType("TD");
    profileResponse.setCompany(companyDTO);
    MultipartFile multipartFile = generateDummyExcelMultipartFile();


    Mockito.when(businessPartnerService.filterByBusinessPartnerCode(BUSINESS_PARTNER_CODE)).thenReturn(profileResponse);
    Mockito.when(
            xBulkFeign.checkPendingBulkRequestsByBusinessPartnerCode(Constants.BULK_UPLOAD_TYPE, BUSINESS_PARTNER_CODE, Constants.ASSEMBLY_REQUEST))
        .thenReturn(new GdnRestSingleResponse<>(null, null, true, new BulkPendingRequestsResponse(), REQUEST_ID));
    Mockito.when(xBulkFeign.createWorkOrder(Mockito.any(BulkProcessUpdateRequest.class)))
        .thenReturn(new GdnBaseRestResponse(null, null, true, REQUEST_ID));

    bulkProcessService.uploadBulkForWorkOrderCreation(USERNAME, Constants.ASSEMBLY_REQUEST, BUSINESS_PARTNER_CODE,
        multipartFile);

    Mockito.verify(businessPartnerService).filterByBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    Mockito.verify(xBulkFeign).createWorkOrder(Mockito.any(BulkProcessUpdateRequest.class));
    Mockito.verify(xBulkFeign)
        .checkPendingBulkRequestsByBusinessPartnerCode(Constants.BULK_UPLOAD_TYPE, BUSINESS_PARTNER_CODE, Constants.ASSEMBLY_REQUEST);

  }

  @Test
  void uploadBulkForWorkOrderCreationSwitchOffTest() throws Exception {
    ReflectionTestUtils.setField(bulkProcessService, "productBundlingEnabled", false);
    ReflectionTestUtils.setField(bulkProcessService, "bundlingAllowedSellerType", "TD");
    ProfileResponse profileResponse = new ProfileResponse();
    CompanyDTO companyDTO = new CompanyDTO();
    companyDTO.setMerchantType("TD");
    profileResponse.setCompany(companyDTO);
    MultipartFile multipartFile = generateDummyExcelMultipartFile();


    Mockito.when(businessPartnerService.filterByBusinessPartnerCode(BUSINESS_PARTNER_CODE)).thenReturn(profileResponse);

    try {
      Assertions.assertThrows(ApplicationRuntimeException.class,
          () -> bulkProcessService.uploadBulkForWorkOrderCreation(USERNAME,
              Constants.ASSEMBLY_REQUEST, BUSINESS_PARTNER_CODE, multipartFile));
    } finally {
      Mockito.verify(businessPartnerService).filterByBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    }

  }

  @Test
  void uploadBulkForWorkOrderCreationSellerTypeTest() throws Exception {
    ReflectionTestUtils.setField(bulkProcessService, "productBundlingEnabled", true);
    ReflectionTestUtils.setField(bulkProcessService, "bundlingAllowedSellerType", "TD");
    ProfileResponse profileResponse = new ProfileResponse();
    CompanyDTO companyDTO = new CompanyDTO();
    companyDTO.setMerchantType("CC");
    profileResponse.setCompany(companyDTO);
    MultipartFile multipartFile = generateDummyExcelMultipartFile();


    Mockito.when(businessPartnerService.filterByBusinessPartnerCode(BUSINESS_PARTNER_CODE)).thenReturn(profileResponse);

    try {
      Assertions.assertThrows(ApplicationRuntimeException.class,
          () -> bulkProcessService.uploadBulkForWorkOrderCreation(USERNAME,
              Constants.ASSEMBLY_REQUEST, BUSINESS_PARTNER_CODE, multipartFile));
    } finally {
      Mockito.verify(businessPartnerService).filterByBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    }

  }

  @Test
  void uploadBulkForWorkOrderCreationInvalidTypeTest() throws Exception {
    MultipartFile multipartFile = generateDummyExcelMultipartFile();
    Assertions.assertThrows(ApplicationRuntimeException.class,
        () -> bulkProcessService.uploadBulkForWorkOrderCreation(USERNAME,
            Constants.ARCHIVE_BULK_PROCESS_TYPE, BUSINESS_PARTNER_CODE, multipartFile));
  }

  @Test
  public void getBulkProcessResponseByProcessCodeTest() {
    GdnRestSingleResponse<BulkProcessResponse> response = new GdnRestSingleResponse<>();
    BulkProcessResponse bulkProcessResponse = new BulkProcessResponse();
    bulkProcessResponse.setBulkProcessCode(BULK_PROCESS_CODE);
    response.setValue(bulkProcessResponse);
    response.setSuccess(true);
    Mockito.when(xBulkFeign.getBulkProcessByProcessCode(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USERNAME,
        BULK_PROCESS_CODE)).thenReturn(response);
    Mockito.when(mandatoryParameterHelper.getRequestId()).thenReturn(REQUEST_ID);
    Mockito.when(mandatoryParameterHelper.getChannelId()).thenReturn(CHANNEL_ID);
    Mockito.when(mandatoryParameterHelper.getClientId()).thenReturn(CLIENT_ID);
    Mockito.when(mandatoryParameterHelper.getUsername()).thenReturn(USERNAME);
    Mockito.when(mandatoryParameterHelper.getStoreId()).thenReturn(STORE_ID);
    bulkProcessService.getBulkProcessResponseByProcessCode(BULK_PROCESS_CODE);
    Mockito.verify(xBulkFeign)
        .getBulkProcessByProcessCode(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USERNAME, BULK_PROCESS_CODE);
    Mockito.verify(mandatoryParameterHelper).getStoreId();
    Mockito.verify(mandatoryParameterHelper).getUsername();
    Mockito.verify(mandatoryParameterHelper).getClientId();
    Mockito.verify(mandatoryParameterHelper).getChannelId();
    Mockito.verify(mandatoryParameterHelper).getRequestId();
  }

  @Test
  public void uploadBulkUpdateMasterInfo() throws Exception {
    pendingRequestsResponse.setPendingRequestsCount(0);
    BulkBasicInfoWebRequest request = new BulkBasicInfoWebRequest();
    request.setFileName(DUMMY_EXCEL_FILE);
    request.setFilePath(DUMMY_EXCEL_FILE);
    when(mandatoryParameterHelper.getStoreId()).thenReturn(STORE_ID);
    when(mandatoryParameterHelper.getChannelId()).thenReturn(CHANNEL_ID);
    when(mandatoryParameterHelper.getClientId()).thenReturn(CLIENT_ID);
    when(mandatoryParameterHelper.getRequestId()).thenReturn(REQUEST_ID);
    when(mandatoryParameterHelper.getUsername()).thenReturn(Constants.USER_NAME);
    when(mandatoryParameterHelper.getIsProductVideoActivated()).thenReturn(true);
    Mockito.when(businessPartnerService.filterByBusinessPartnerCode(Constants.BUSINESS_PARTNER_CODE))
        .thenReturn(profileResponse);
    Mockito.when(
            this.xBulkFeign.uploadBulkBasicInfoFile(Mockito.eq(STORE_ID), Mockito.eq(CHANNEL_ID), Mockito.eq(CLIENT_ID),
                Mockito.eq(REQUEST_ID), Mockito.eq(USER_NAME), Mockito.any(BulkBasicInfoRequest.class)))
        .thenReturn(gdnBaseRestResponse);
    Mockito.when(
            xBulkFeign.checkPendingBulkRequestsByBusinessPartnerCode(Constants.BULK_UPLOAD_TYPE, BUSINESS_PARTNER_CODE,
                Constants.BULK_PROCESS_MASTER_INFO_UPDATE))
        .thenReturn(new GdnRestSingleResponse<>(pendingRequestsResponse, StringUtils.EMPTY));
    this.bulkProcessService.uploadBulkUpdateMasterInfo(BUSINESS_PARTNER_CODE, USERNAME, request);
    Mockito.verify(businessPartnerService).filterByBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    Mockito.verify(xBulkFeign)
        .uploadBulkBasicInfoFile(Mockito.eq(STORE_ID), Mockito.eq(CHANNEL_ID), Mockito.eq(CLIENT_ID),
            Mockito.eq(REQUEST_ID), Mockito.eq(USER_NAME), bulkBasicInfoRequestArgumentCaptor.capture());
    verify(xBulkFeign).checkPendingBulkRequestsByBusinessPartnerCode(Constants.BULK_UPLOAD_TYPE, BUSINESS_PARTNER_CODE,
        Constants.BULK_PROCESS_MASTER_INFO_UPDATE);
    assertEquals(BUSINESS_PARTNER_CODE, bulkBasicInfoRequestArgumentCaptor.getValue().getBusinessPartnerCode());
    assertEquals(DUMMY_EXCEL_FILE, bulkBasicInfoRequestArgumentCaptor.getValue().getFileName());
    assertEquals(USERNAME, bulkBasicInfoRequestArgumentCaptor.getValue().getUpdatedBy());
    verify(mandatoryParameterHelper).getStoreId();
    verify(mandatoryParameterHelper).getChannelId();
    verify(mandatoryParameterHelper).getClientId();
    verify(mandatoryParameterHelper).getRequestId();
    verify(mandatoryParameterHelper).getUsername();
    verify(mandatoryParameterHelper, times(2)).getIsProductVideoActivated();
  }

  @Test
  public void uploadBulkUpdateMasterInfoInactive() throws Exception {
    pendingRequestsResponse.setPendingRequestsCount(0);
    profileResponse.setMerchantStatus(INACTIVE_STATUS);
    BulkBasicInfoWebRequest request = new BulkBasicInfoWebRequest();
    request.setFileName(DUMMY_EXCEL_FILE);
    request.setFilePath(DUMMY_EXCEL_FILE);
    when(mandatoryParameterHelper.getStoreId()).thenReturn(STORE_ID);
    when(mandatoryParameterHelper.getChannelId()).thenReturn(CHANNEL_ID);
    when(mandatoryParameterHelper.getClientId()).thenReturn(CLIENT_ID);
    when(mandatoryParameterHelper.getRequestId()).thenReturn(REQUEST_ID);
    when(mandatoryParameterHelper.getUsername()).thenReturn(Constants.USER_NAME);
    try {

      Mockito.when(businessPartnerService.filterByBusinessPartnerCode(Constants.BUSINESS_PARTNER_CODE))
          .thenReturn(profileResponse);
      Mockito.when(
              this.xBulkFeign.uploadBulkBasicInfoFile(Mockito.eq(STORE_ID), Mockito.eq(CHANNEL_ID), Mockito.eq(CLIENT_ID),
                  Mockito.eq(REQUEST_ID), Mockito.eq(USER_NAME), Mockito.any(BulkBasicInfoRequest.class)))
          .thenReturn(gdnBaseRestResponse);
      Mockito.when(
              xBulkFeign.checkPendingBulkRequestsByBusinessPartnerCode(Constants.BULK_UPLOAD_TYPE, BUSINESS_PARTNER_CODE,
                  Constants.BULK_PROCESS_MASTER_INFO_UPDATE))
          .thenReturn(new GdnRestSingleResponse<>(pendingRequestsResponse, StringUtils.EMPTY));
      this.bulkProcessService.uploadBulkUpdateMasterInfo(BUSINESS_PARTNER_CODE, USERNAME, request);
    } catch (Exception e) {

      Mockito.verify(businessPartnerService).filterByBusinessPartnerCode(BUSINESS_PARTNER_CODE);
      verify(xBulkFeign).checkPendingBulkRequestsByBusinessPartnerCode(Constants.BULK_UPLOAD_TYPE,
          BUSINESS_PARTNER_CODE, Constants.BULK_PROCESS_MASTER_INFO_UPDATE);
    }
  }

  @Test
  public void uploadBulkUpdateMasterInfoWithEmptyBusinessPartner() throws Exception {
    pendingRequestsResponse.setPendingRequestsCount(0);
    BulkBasicInfoWebRequest request = new BulkBasicInfoWebRequest();
    request.setFileName(DUMMY_EXCEL_FILE);
    request.setFilePath(DUMMY_EXCEL_FILE);
    when(mandatoryParameterHelper.getStoreId()).thenReturn(STORE_ID);
    when(mandatoryParameterHelper.getChannelId()).thenReturn(CHANNEL_ID);
    when(mandatoryParameterHelper.getClientId()).thenReturn(CLIENT_ID);
    when(mandatoryParameterHelper.getRequestId()).thenReturn(REQUEST_ID);
    when(mandatoryParameterHelper.getUsername()).thenReturn(Constants.USER_NAME);
    Mockito.when(businessPartnerService.filterByBusinessPartnerCode(Constants.BUSINESS_PARTNER_CODE)).thenReturn(null);
    try {

      Mockito.when(
              this.xBulkFeign.uploadBulkBasicInfoFile(Mockito.eq(STORE_ID), Mockito.eq(CHANNEL_ID), Mockito.eq(CLIENT_ID),
                  Mockito.eq(REQUEST_ID), Mockito.eq(USER_NAME), Mockito.any(BulkBasicInfoRequest.class)))
          .thenReturn(gdnBaseRestResponse);
      Mockito.when(
              xBulkFeign.checkPendingBulkRequestsByBusinessPartnerCode(Constants.BULK_UPLOAD_TYPE, BUSINESS_PARTNER_CODE,
                  Constants.BULK_PROCESS_MASTER_INFO_UPDATE))
          .thenReturn(new GdnRestSingleResponse<>(pendingRequestsResponse, StringUtils.EMPTY));
      this.bulkProcessService.uploadBulkUpdateMasterInfo(BUSINESS_PARTNER_CODE, USERNAME, request);
    } catch (Exception e) {
      Mockito.verify(businessPartnerService).filterByBusinessPartnerCode(BUSINESS_PARTNER_CODE);
      verify(xBulkFeign).checkPendingBulkRequestsByBusinessPartnerCode(Constants.BULK_UPLOAD_TYPE,
          BUSINESS_PARTNER_CODE, Constants.BULK_PROCESS_MASTER_INFO_UPDATE);
    }
  }


  @Test
  public void uploadV2Test() throws Exception {
    fileNames.clear();
    fileNames.add(EXCEL_FILE);
    pendingRequestsResponse.setPendingRequestsCount(0);
    List<MultipartFile> multipartFiles = new ArrayList<>();
    multipartFiles.add(generateInvalidUpsertXlsm());
    Mockito.when(xBulkFeign.upload(Mockito.any(BulkProcessUploadRequest.class)))
        .thenReturn(new GdnBaseRestResponse(true));
    Mockito.when(businessPartnerService.filterByBusinessPartnerCode(Constants.BUSINESS_PARTNER_CODE))
        .thenReturn(profileResponse);
    Mockito.when(xBulkFeign
            .checkPendingBulkRequestsByBusinessPartnerCode(Constants.BULK_UPLOAD_TYPE, BUSINESS_PARTNER_CODE, Constants.BULK_PRODUCT_CREATION_TYPE))
        .thenReturn(new GdnRestSingleResponse<>(pendingRequestsResponse, StringUtils.EMPTY));
    Mockito.when(fileStorageService.downloadMultiPartFile(USERNAME,
        fileNames)).thenReturn(multipartFiles);
    when(userPicService.fetchAccessiblePickupPointCodes(profileResponse)).thenReturn(
        Collections.singleton(PICKUP_POINT_CODE));
    this.bulkProcessService.uploadV2(BUSINESS_PARTNER_CODE, USERNAME, fileNames, "");
    Mockito.verify(xBulkFeign).upload(bulkProcessUploadRequestArgumentCaptor.capture());
    Mockito.verify(businessPartnerService).filterByBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    verify(xBulkFeign).checkPendingBulkRequestsByBusinessPartnerCode(Constants.BULK_UPLOAD_TYPE, BUSINESS_PARTNER_CODE, Constants.BULK_PRODUCT_CREATION_TYPE);
    assertEquals(BULK_PROCESS_TYPE, bulkProcessUploadRequestArgumentCaptor.getValue().getBulkProcessType());
    assertEquals(BUSINESS_PARTNER_CODE, bulkProcessUploadRequestArgumentCaptor.getValue().getBusinessPartnerCode());
    assertNotNull(bulkProcessUploadRequestArgumentCaptor.getValue().getArgs());
    assertNotNull(bulkProcessUploadRequestArgumentCaptor.getValue().getFiles());
    assertNotNull(bulkProcessUploadRequestArgumentCaptor.getValue().getFiles());
    Mockito.verify(userPicService).fetchAccessiblePickupPointCodes(profileResponse);
    Mockito.verify(mandatoryParameterHelper).isExternalOnly();
  }

  @Test
  public void uploadExternalFiles() throws Exception {
    ReflectionTestUtils.setField(bulkProcessService, "fileMapKeys", "file1,file2");
    pendingRequestsResponse.setBulkUpdateStatusFlag(false);
    pendingRequestsResponse.setPendingRequestsCount(0);
    externalFiles.put("file1", "file1.xlsx");
    externalFiles.put("file2", "file2.xlsx");
    Mockito.when(businessPartnerService.filterByBusinessPartnerCode(BUSINESS_PARTNER_CODE))
        .thenReturn(profileResponse);
    Mockito.when(userPicService.fetchAccessiblePickupPointCodes(profileResponse))
        .thenReturn(Collections.singleton(PICKUP_POINT_CODE));
    Mockito.when(
            xBulkFeign.checkPendingBulkRequestsByBusinessPartnerCode(Constants.BULK_UPLOAD_TYPE,
                BUSINESS_PARTNER_CODE, Constants.BULK_PRODUCT_EXTERNAL_CREATION))
        .thenReturn(new GdnRestSingleResponse<>(pendingRequestsResponse, StringUtils.EMPTY));
    Mockito.when(xBulkFeign.externalUpload(
            RequestHelper.toBulkProcessExternalUploadRequest("zip.zip", externalFiles,
                BULK_PROCESS_CODE, PICKUP_POINT_CODE, null, BUSINESS_PARTNER_CODE)))
        .thenReturn(new GdnBaseRestResponse(true));
    this.bulkProcessService.uploadExternalFiles(BUSINESS_PARTNER_CODE, USERNAME, "zip.zip",
        externalFiles, PICKUP_POINT_CODE, BULK_PROCESS_CODE);
    Mockito.verify(xBulkFeign).externalUpload(
        RequestHelper.toBulkProcessExternalUploadRequest("zip.zip", externalFiles,
            BULK_PROCESS_CODE, PICKUP_POINT_CODE, null, BUSINESS_PARTNER_CODE));
    Mockito.verify(xBulkFeign)
        .checkPendingBulkRequestsByBusinessPartnerCode(Constants.BULK_UPLOAD_TYPE,
            BUSINESS_PARTNER_CODE, Constants.BULK_PRODUCT_EXTERNAL_CREATION);
    Mockito.verify(businessPartnerService).filterByBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    Mockito.verify(userPicService).fetchAccessiblePickupPointCodes(profileResponse);
    Mockito.verify(mandatoryParameterHelper).isExternalOnly();
  }

  @Test
  public void uploadExternalEmptyFiles() throws Exception {
    ReflectionTestUtils.setField(bulkProcessService, "fileMapKeys", "file1,file2");
    pendingRequestsResponse.setBulkUpdateStatusFlag(false);
    pendingRequestsResponse.setPendingRequestsCount(0);
    externalFiles.put("file1", "file1.xlsx");
    externalFiles.put("file2", "file2.xlsx");
    Mockito.when(businessPartnerService.filterByBusinessPartnerCode(BUSINESS_PARTNER_CODE))
      .thenReturn(profileResponse);
    Mockito.when(userPicService.fetchAccessiblePickupPointCodes(profileResponse))
      .thenReturn(new HashSet<>());
    Mockito.when(
        xBulkFeign.checkPendingBulkRequestsByBusinessPartnerCode(Constants.BULK_UPLOAD_TYPE,
          BUSINESS_PARTNER_CODE, Constants.BULK_PRODUCT_EXTERNAL_CREATION))
      .thenReturn(new GdnRestSingleResponse<>(pendingRequestsResponse, StringUtils.EMPTY));
    Mockito.when(xBulkFeign.externalUpload(
        RequestHelper.toBulkProcessExternalUploadRequest("zip.zip", externalFiles,
          BULK_PROCESS_CODE, PICKUP_POINT_CODE, null, BUSINESS_PARTNER_CODE)))
      .thenReturn(new GdnBaseRestResponse(true));
    this.bulkProcessService.uploadExternalFiles(BUSINESS_PARTNER_CODE, USERNAME, "zip.zip",
      externalFiles, PICKUP_POINT_CODE, BULK_PROCESS_CODE);
    Mockito.verify(xBulkFeign).externalUpload(
      RequestHelper.toBulkProcessExternalUploadRequest("zip.zip", externalFiles,
        BULK_PROCESS_CODE, PICKUP_POINT_CODE, null, BUSINESS_PARTNER_CODE));
    Mockito.verify(xBulkFeign)
      .checkPendingBulkRequestsByBusinessPartnerCode(Constants.BULK_UPLOAD_TYPE,
        BUSINESS_PARTNER_CODE, Constants.BULK_PRODUCT_EXTERNAL_CREATION);
    Mockito.verify(businessPartnerService).filterByBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    Mockito.verify(userPicService).fetchAccessiblePickupPointCodes(profileResponse);
    Mockito.verify(mandatoryParameterHelper).isExternalOnly();
  }

  @Test
  public void uploadExternalFiles_NoZipFile() throws Exception {
    ReflectionTestUtils.setField(bulkProcessService, "fileMapKeys", "file1,file2");
    pendingRequestsResponse.setBulkUpdateStatusFlag(false);
    pendingRequestsResponse.setPendingRequestsCount(0);
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class,
          () -> this.bulkProcessService.uploadExternalFiles(BUSINESS_PARTNER_CODE, USERNAME, StringUtils.EMPTY,
              externalFiles, PICKUP_POINT_CODE, BULK_PROCESS_CODE));
    }finally {

    }
  }
  @Test
  public void uploadExternalFilesEmptyFiles() throws Exception {
    ReflectionTestUtils.setField(bulkProcessService, "fileMapKeys", "file1,file2");
    pendingRequestsResponse.setBulkUpdateStatusFlag(false);
    pendingRequestsResponse.setPendingRequestsCount(0);
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class,
      () -> this.bulkProcessService.uploadExternalFiles(BUSINESS_PARTNER_CODE, USERNAME, "zip.zip",
          externalFiles, PICKUP_POINT_CODE, BULK_PROCESS_CODE));
    }finally {

    }
  }

  @Test
  public void uploadExternalFiles_filesDontMatch() throws Exception {
    ReflectionTestUtils.setField(bulkProcessService, "fileMapKeys", "file,file1");
    externalFiles.put("file2", "file2.xlsx");
    pendingRequestsResponse.setBulkUpdateStatusFlag(false);
    pendingRequestsResponse.setPendingRequestsCount(0);
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class,
          () -> this.bulkProcessService.uploadExternalFiles(BUSINESS_PARTNER_CODE, USERNAME,
              "zip.zip", externalFiles, PICKUP_POINT_CODE, BULK_PROCESS_CODE));
    } finally {

    }
  }
  @Test
  public void uploadExternalFilesNoPickupPoint() throws Exception {
    ReflectionTestUtils.setField(bulkProcessService, "fileMapKeys", "file1,file2");
    pendingRequestsResponse.setBulkUpdateStatusFlag(false);
    pendingRequestsResponse.setPendingRequestsCount(0);
    externalFiles.put("file1", "file1.xlsx");
    externalFiles.put("file2", "file2.xlsx");
    Mockito.when(businessPartnerService.filterByBusinessPartnerCode(BUSINESS_PARTNER_CODE))
        .thenReturn(profileResponse);
    Mockito.when(
            xBulkFeign.checkPendingBulkRequestsByBusinessPartnerCode(Constants.BULK_UPLOAD_TYPE,
                BUSINESS_PARTNER_CODE, Constants.BULK_PRODUCT_EXTERNAL_CREATION))
        .thenReturn(new GdnRestSingleResponse<>(pendingRequestsResponse, StringUtils.EMPTY));
    Mockito.when(xBulkFeign.externalUpload(
            RequestHelper.toBulkProcessExternalUploadRequest("zip.zip", externalFiles,
                BULK_PROCESS_CODE, StringUtils.EMPTY, null, BUSINESS_PARTNER_CODE)))
        .thenReturn(new GdnBaseRestResponse(true));
    this.bulkProcessService.uploadExternalFiles(BUSINESS_PARTNER_CODE, USERNAME, "zip.zip",
        externalFiles, StringUtils.EMPTY, BULK_PROCESS_CODE);
    Mockito.verify(xBulkFeign).externalUpload(
        RequestHelper.toBulkProcessExternalUploadRequest("zip.zip", externalFiles,
            BULK_PROCESS_CODE, StringUtils.EMPTY, null, BUSINESS_PARTNER_CODE));
    Mockito.verify(xBulkFeign)
        .checkPendingBulkRequestsByBusinessPartnerCode(Constants.BULK_UPLOAD_TYPE,
            BUSINESS_PARTNER_CODE, Constants.BULK_PRODUCT_EXTERNAL_CREATION);
    Mockito.verify(businessPartnerService).filterByBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    Mockito.verify(mandatoryParameterHelper).isExternalOnly();
  }

  @Test
  public void uploadExternalFiles_inaccessible_pickupPoint() throws Exception {
    ReflectionTestUtils.setField(bulkProcessService, "fileMapKeys", "file1,file2");
    pendingRequestsResponse.setBulkUpdateStatusFlag(false);
    pendingRequestsResponse.setPendingRequestsCount(0);
    externalFiles.put("file1", "file1.xlsx");
    externalFiles.put("file2", "file2.xlsx");
    Mockito.when(businessPartnerService.filterByBusinessPartnerCode(BUSINESS_PARTNER_CODE))
        .thenReturn(profileResponse);
    Mockito.when(userPicService.fetchAccessiblePickupPointCodes(profileResponse))
        .thenReturn(Collections.singleton(StringUtils.EMPTY));
    try {
      Assertions.assertThrows(ValidationException.class,
          () -> this.bulkProcessService.uploadExternalFiles(BUSINESS_PARTNER_CODE, USERNAME,
              "zip.zip", externalFiles, PICKUP_POINT_CODE, BULK_PROCESS_CODE));
    } finally {
      Mockito.verify(businessPartnerService).filterByBusinessPartnerCode(BUSINESS_PARTNER_CODE);
      Mockito.verify(userPicService).fetchAccessiblePickupPointCodes(profileResponse);
    }
  }

  @Test
  public void uploadV2WithXlsmFileTest() throws Exception {
    fileNames.clear();
    fileNames.add(EXCEL_FILE_XLSM);
    fileNames.add(ZIP_FILE);
    pendingRequestsResponse.setPendingRequestsCount(0);
    List<MultipartFile> multipartFiles = new ArrayList<>();
    multipartFiles.add(generateDummyExcelMultipartFileXlsm());
    Mockito.when(xBulkFeign.upload(Mockito.any(BulkProcessUploadRequest.class)))
        .thenReturn(new GdnBaseRestResponse(true));
    Mockito.when(businessPartnerService.filterByBusinessPartnerCode(Constants.BUSINESS_PARTNER_CODE))
        .thenReturn(profileResponse);
    Mockito.when(xBulkFeign
            .checkPendingBulkRequestsByBusinessPartnerCode(Constants.BULK_UPLOAD_TYPE, BUSINESS_PARTNER_CODE, Constants.BULK_PRODUCT_CREATION_TYPE))
        .thenReturn(new GdnRestSingleResponse<>(pendingRequestsResponse, StringUtils.EMPTY));
    Mockito.when(fileStorageService.downloadMultiPartFile(USERNAME,
        fileNames)).thenReturn(multipartFiles);
    when(userPicService.fetchAccessiblePickupPointCodes(profileResponse)).thenReturn(
        Collections.singleton(PICKUP_POINT_CODE));
    this.bulkProcessService.uploadV2(BUSINESS_PARTNER_CODE, USERNAME, fileNames, "");
    Mockito.verify(xBulkFeign).upload(bulkProcessUploadRequestArgumentCaptor.capture());
    Mockito.verify(businessPartnerService).filterByBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    verify(xBulkFeign).checkPendingBulkRequestsByBusinessPartnerCode(Constants.BULK_UPLOAD_TYPE, BUSINESS_PARTNER_CODE, Constants.BULK_PRODUCT_CREATION_TYPE);
    assertEquals(BULK_PROCESS_TYPE, bulkProcessUploadRequestArgumentCaptor.getValue().getBulkProcessType());
    assertEquals(BUSINESS_PARTNER_CODE, bulkProcessUploadRequestArgumentCaptor.getValue().getBusinessPartnerCode());
    assertNotNull(bulkProcessUploadRequestArgumentCaptor.getValue().getArgs());
    assertNotNull(bulkProcessUploadRequestArgumentCaptor.getValue().getFiles());
    assertNotNull(bulkProcessUploadRequestArgumentCaptor.getValue().getFiles());
    Mockito.verify(userPicService).fetchAccessiblePickupPointCodes(profileResponse);
    Mockito.verify(mandatoryParameterHelper).isExternalOnly();
  }

  @Test
  public void uploadV2WithOtherMarketPlaceFileTest() throws Exception {
    fileNames.clear();
    fileNames.add(EXCEL_FILE_XLSM);
    fileNames.add(ZIP_FILE);
    pendingRequestsResponse.setPendingRequestsCount(0);
    List<MultipartFile> multipartFiles = new ArrayList<>();
    multipartFiles.add(generateDummyExcelMultipartFileXlsm());
    Mockito.when(xBulkFeign.upload(Mockito.any(BulkProcessUploadRequest.class)))
        .thenReturn(new GdnBaseRestResponse(true));
    Mockito.when(
            businessPartnerService.filterByBusinessPartnerCode(Constants.BUSINESS_PARTNER_CODE))
        .thenReturn(profileResponse);
    Mockito.when(
            xBulkFeign.checkPendingBulkRequestsByBusinessPartnerCode(Constants.BULK_UPLOAD_TYPE,
                BUSINESS_PARTNER_CODE, Constants.CONVERTED_PRODUCT_CREATION_UPLOAD))
        .thenReturn(new GdnRestSingleResponse<>(pendingRequestsResponse, StringUtils.EMPTY));
    Mockito.when(fileStorageService.downloadMultiPartFile(USERNAME, fileNames))
        .thenReturn(multipartFiles);
    when(userPicService.fetchAccessiblePickupPointCodes(profileResponse)).thenReturn(
        Collections.singleton(PICKUP_POINT_CODE));
    this.bulkProcessService.uploadV2(BUSINESS_PARTNER_CODE, USERNAME, fileNames,
        Constants.CONVERTED_PRODUCT_CREATION_UPLOAD);
    Mockito.verify(xBulkFeign).upload(bulkProcessUploadRequestArgumentCaptor.capture());
    Mockito.verify(businessPartnerService).filterByBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    verify(xBulkFeign).checkPendingBulkRequestsByBusinessPartnerCode(Constants.BULK_UPLOAD_TYPE,
        BUSINESS_PARTNER_CODE, Constants.CONVERTED_PRODUCT_CREATION_UPLOAD);
    assertEquals(BULK_PROCESS_TYPE,
        bulkProcessUploadRequestArgumentCaptor.getValue().getBulkProcessType());
    assertEquals("true", bulkProcessUploadRequestArgumentCaptor.getValue().getArgs()
        .get(Constants.CONVERTED_PRODUCT_CREATION_UPLOAD));
    assertEquals(BUSINESS_PARTNER_CODE,
        bulkProcessUploadRequestArgumentCaptor.getValue().getBusinessPartnerCode());
    assertNotNull(bulkProcessUploadRequestArgumentCaptor.getValue().getArgs());
    assertNotNull(bulkProcessUploadRequestArgumentCaptor.getValue().getFiles());
    Mockito.verify(userPicService).fetchAccessiblePickupPointCodes(profileResponse);
    Mockito.verify(mandatoryParameterHelper).isExternalOnly();
  }

  @Test
  public void uploadV2BulkUpsertTest() throws Exception {
    ReflectionTestUtils.setField(bulkProcessService, "multiPickupPointEnabled", true);
    fileNames.clear();
    fileNames.add(EXCEL_UPSERT_FILE);
    List<MultipartFile> multipartFiles = new ArrayList<>();
    multipartFiles.add(generateDummyExcelMultipartFileUpsertXlsm());
    pendingRequestsResponse.setPendingRequestsCount(0);
    Mockito.when(businessPartnerService.filterByBusinessPartnerCode(Constants.BUSINESS_PARTNER_CODE))
        .thenReturn(profileResponse);
    Mockito.when(fileStorageService.downloadMultiPartFile(USERNAME,
        fileNames)).thenReturn(multipartFiles);
    Mockito.when(xBulkFeign.uploadForBulkUpsertOfflineItems(Mockito.any()))
        .thenReturn(new GdnBaseRestResponse(true));
    Mockito.when(xBulkFeign.checkPendingBulkRequestsByBusinessPartnerCode(Constants.BULK_UPLOAD_TYPE, BUSINESS_PARTNER_CODE,
            BulkProcessType.INSTANT_PICKUP_PRODUCT_UPSERT.getValue()))
        .thenReturn(new GdnRestSingleResponse<>(pendingRequestsResponse, StringUtils.EMPTY));
    this.bulkProcessService.uploadV2(BUSINESS_PARTNER_CODE, USERNAME, fileNames, BULK_UPSERT);
    Mockito.verify(xBulkFeign).uploadForBulkUpsertOfflineItems(bulkProcessUpsertOfflineItemRequestArgumentCaptor.capture());
    Mockito.verify(businessPartnerService).filterByBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    verify(xBulkFeign).checkPendingBulkRequestsByBusinessPartnerCode(Constants.BULK_UPLOAD_TYPE, BUSINESS_PARTNER_CODE,
        BulkProcessType.INSTANT_PICKUP_PRODUCT_UPSERT.getValue());
    assertEquals(BulkProcessType.INSTANT_PICKUP_PRODUCT.getValue(), bulkProcessUpsertOfflineItemRequestArgumentCaptor.getValue().getBulkProcessType());
    assertEquals(BUSINESS_PARTNER_CODE, bulkProcessUpsertOfflineItemRequestArgumentCaptor.getValue().getBusinessPartnerCode());
    verify(userPicService).fetchAccessiblePickupPointCodes(profileResponse);
    assertEquals(BulkProcessType.INSTANT_PICKUP_PRODUCT.getValue(),
        bulkProcessUpsertOfflineItemRequestArgumentCaptor.getValue().getBulkProcessType());
    assertEquals(BUSINESS_PARTNER_CODE,
        bulkProcessUpsertOfflineItemRequestArgumentCaptor.getValue().getBusinessPartnerCode());
  }

  @Test
  public void uploadExcelFile() throws Exception {
    ReflectionTestUtils.setField(bulkProcessService, "multiPickupPointEnabled", true);
    fileNames.clear();
    fileNames.add(EXCEL_UPSERT_FILE);
    List<MultipartFile> multipartFiles = new ArrayList<>();
    multipartFiles.add(generateDummyExcelMultipartFileUpsertXlsm());
    pendingRequestsResponse.setPendingRequestsCount(0);
    Mockito.when(businessPartnerService.filterByBusinessPartnerCode(Constants.BUSINESS_PARTNER_CODE))
        .thenReturn(profileResponse);
    Mockito.when(fileStorageService.downloadMultiPartFile(USERNAME,
        fileNames)).thenReturn(multipartFiles);
    Mockito.when(xBulkFeign.uploadForBulkUpsertOfflineItems(Mockito.any()))
        .thenReturn(new GdnBaseRestResponse(true));
    Mockito.when(xBulkFeign.checkPendingBulkRequestsByBusinessPartnerCode(Constants.BULK_UPLOAD_TYPE, BUSINESS_PARTNER_CODE,
            BulkProcessType.INSTANT_PICKUP_PRODUCT_UPSERT.getValue()))
        .thenReturn(new GdnRestSingleResponse<>(pendingRequestsResponse, StringUtils.EMPTY));
    this.bulkProcessService.uploadExcelFile(BUSINESS_PARTNER_CODE, USERNAME, fileNames, "");
    Mockito.verify(xBulkFeign).uploadForBulkUpsertOfflineItems(bulkProcessUpsertOfflineItemRequestArgumentCaptor.capture());
    Mockito.verify(businessPartnerService).filterByBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    verify(xBulkFeign).checkPendingBulkRequestsByBusinessPartnerCode(Constants.BULK_UPLOAD_TYPE, BUSINESS_PARTNER_CODE,
        BulkProcessType.INSTANT_PICKUP_PRODUCT_UPSERT.getValue());
    assertEquals(BulkProcessType.INSTANT_PICKUP_PRODUCT.getValue(), bulkProcessUpsertOfflineItemRequestArgumentCaptor.getValue().getBulkProcessType());
    assertEquals(BUSINESS_PARTNER_CODE, bulkProcessUpsertOfflineItemRequestArgumentCaptor.getValue().getBusinessPartnerCode());
    verify(userPicService).fetchAccessiblePickupPointCodes(profileResponse);
    assertEquals(BulkProcessType.INSTANT_PICKUP_PRODUCT.getValue(),
        bulkProcessUpsertOfflineItemRequestArgumentCaptor.getValue().getBulkProcessType());
    assertEquals(BUSINESS_PARTNER_CODE,
        bulkProcessUpsertOfflineItemRequestArgumentCaptor.getValue().getBusinessPartnerCode());
  }

  @Test
  public void uploadExcelFileV2() throws Exception {
    ReflectionTestUtils.setField(bulkProcessService, "avoidRedundantDownloadInBulkCreation", true);
    ReflectionTestUtils.setField(bulkProcessService, "multiPickupPointEnabled", true);
    fileNames.clear();
    fileNames.add(EXCEL_UPSERT_FILE);
    List<MultipartFile> multipartFiles = new ArrayList<>();
    multipartFiles.add(generateDummyExcelMultipartFileUpsertXlsm());
    pendingRequestsResponse.setPendingRequestsCount(0);
    Mockito.when(businessPartnerService.filterByBusinessPartnerCode(Constants.BUSINESS_PARTNER_CODE))
        .thenReturn(profileResponse);
    Mockito.when(fileStorageService.downloadMultiPartFile(USERNAME,
        fileNames)).thenReturn(multipartFiles);
    Mockito.when(xBulkFeign.uploadForBulkUpsertOfflineItems(Mockito.any()))
        .thenReturn(new GdnBaseRestResponse(true));
    Mockito.when(xBulkFeign.checkPendingBulkRequestsByBusinessPartnerCode(Constants.BULK_UPLOAD_TYPE, BUSINESS_PARTNER_CODE,
            BulkProcessType.INSTANT_PICKUP_PRODUCT_UPSERT.getValue()))
        .thenReturn(new GdnRestSingleResponse<>(pendingRequestsResponse, StringUtils.EMPTY));
    this.bulkProcessService.uploadExcelFile(BUSINESS_PARTNER_CODE, USERNAME, fileNames,
      Constants.BULK_UPSERT);
    Mockito.verify(xBulkFeign).uploadForBulkUpsertOfflineItems(bulkProcessUpsertOfflineItemRequestArgumentCaptor.capture());
    Mockito.verify(businessPartnerService).filterByBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    verify(xBulkFeign).checkPendingBulkRequestsByBusinessPartnerCode(Constants.BULK_UPLOAD_TYPE, BUSINESS_PARTNER_CODE,
        BulkProcessType.INSTANT_PICKUP_PRODUCT_UPSERT.getValue());
    assertEquals(BulkProcessType.INSTANT_PICKUP_PRODUCT.getValue(), bulkProcessUpsertOfflineItemRequestArgumentCaptor.getValue().getBulkProcessType());
    assertEquals(BUSINESS_PARTNER_CODE, bulkProcessUpsertOfflineItemRequestArgumentCaptor.getValue().getBusinessPartnerCode());
    verify(userPicService).fetchAccessiblePickupPointCodes(profileResponse);
    assertEquals(BulkProcessType.INSTANT_PICKUP_PRODUCT.getValue(),
        bulkProcessUpsertOfflineItemRequestArgumentCaptor.getValue().getBulkProcessType());
    assertEquals(BUSINESS_PARTNER_CODE,
        bulkProcessUpsertOfflineItemRequestArgumentCaptor.getValue().getBusinessPartnerCode());
  }


}