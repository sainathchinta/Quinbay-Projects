package com.gdn.partners.pcu.internal.web.controller;

import static org.hamcrest.Matchers.equalTo;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.partners.pcu.internal.client.helper.ClientParameterHelper;
import com.gdn.partners.pcu.internal.client.model.request.BrandAuthDeleteRequest;
import com.gdn.partners.pcu.internal.model.BrandAuthoriseApiPath;
import com.gdn.partners.pcu.internal.model.Constants;
import com.gdn.partners.pcu.internal.model.ErrorMessages;
import com.gdn.partners.pcu.internal.service.BrandAuthorisationService;
import com.gdn.partners.pcu.internal.service.impl.util.ConverterUtil;
import com.gdn.partners.pcu.internal.web.helper.TestHelper;
import com.gdn.partners.pcu.internal.web.model.request.BrandAuthCreateWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.BrandAuthHistoryWebRequest;
import com.gdn.partners.pcu.internal.web.model.response.BrandAuthCreateWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.BrandAuthorisationDetailWebResponse;
import com.gdn.x.productcategorybase.BrandAuthorisationStatus;
import com.gdn.x.productcategorybase.dto.brand.BrandAuthCreateRequest;
import com.gdn.x.productcategorybase.dto.brand.BrandAuthHistoryRequest;
import com.gdn.x.productcategorybase.dto.brand.BrandAuthHistoryResponse;

import java.io.File;
import java.io.IOException;
import java.net.URI;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;

import org.apache.commons.io.FileUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.http.client.utils.URIBuilder;
import org.hamcrest.CoreMatchers;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.data.domain.PageImpl;
import org.springframework.http.MediaType;
import org.springframework.mock.web.MockMultipartFile;
import org.springframework.test.util.ReflectionTestUtils;
import org.springframework.test.web.servlet.request.MockHttpServletRequestBuilder;
import org.springframework.test.web.servlet.request.MockMvcRequestBuilders;
import org.springframework.test.web.servlet.result.MockMvcResultMatchers;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;

import java.util.List;
import java.util.Objects;

import com.gdn.partners.pcu.internal.web.model.request.BrandAuthWebRequest;
import com.gdn.x.productcategorybase.dto.brand.BrandAuthFilterRequest;
import com.gdn.x.productcategorybase.dto.request.BrandAuthUpdateRequest;

@AutoConfigureMockMvc
@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
public class BrandAuthorisationControllerTest extends TestHelper {

  private static final String BRAND_CODE = "brandCode";
  private static final String BRAND_NAME = "brandName";
  private static final String SELLER_CODE = "sellerCode";
  private static final String SELLER_NAME = "sellerName";
  private static final String USERNAME = "username";
  private static final String STORE_ID = "10001";
  private static final String REQUEST_ID = "requestId";
  private static final int PAGE = 0;
  private static final int SIZE = 10;
  private static final String OLD_VALUE_AUTH = "Inactive";
  private static final String NEW_VALUE_AUTH = "Active";
  private static final String ACTIVITY_AUTH = "Change status";
  private static final ObjectMapper OBJECT_MAPPER = new ObjectMapper();
  private static final String AUTH_DETAIL_API_PATH = "/detail/" + BRAND_CODE;
  private static final String BRAND_AUTH_ADD = "BRAND_AUTH_ADD";
  private static final String BRAND_AUTH_DELETE = "BRAND_AUTH_DELETE";
  private BrandAuthCreateWebResponse brandAuthCreateWebResponse;
  private ObjectMapper objectMapper = new ObjectMapper();
  private BrandAuthCreateWebRequest brandAuthCreateWebRequest;
  private static final String DELETE_API_PATH = "/delete";
  private byte[] fileContent;
  private MockMultipartFile multipartFile;
  private static final String FILE = "/filestore/originalFilename.pdf";
  private static final ClassLoader CLASS_LOADER = ClassLoader.getSystemClassLoader();
  private static final String BASE_DIRECTORY = Objects.requireNonNull(
    CLASS_LOADER.getResource(org.apache.commons.lang.StringUtils.EMPTY)).getPath();
  private static final String PATH = BASE_DIRECTORY + "path";
  private static final String ORIGINAL_FILENAME = "originalFilename.pdf";

  private BrandAuthorisationDetailWebResponse brandAuthorisationDetailWebResponse;
  private GdnRestListResponse<BrandAuthHistoryResponse> historyResponse =
    new GdnRestListResponse<>();;
  private BrandAuthHistoryRequest brandAuthHistoryRequest;
  private BrandAuthHistoryWebRequest brandAuthHistoryWebRequest;
  private BrandAuthHistoryResponse brandAuthHistoryResponse;
  private List<BrandAuthHistoryResponse> brandAuthHistoryResponseList = new ArrayList<>();
  private List<BrandAuthDeleteRequest> brandAuthDeleteRequestList = new ArrayList<>();

  @Mock
  private ClientParameterHelper clientParameterHelper;

  @Mock
  private BrandAuthorisationService brandAuthorisationService;

  @Mock
  private ConverterUtil converterUtil;

  @InjectMocks
  BrandAuthorisationController brandAuthoriseController;

  @BeforeEach
  public void setUp() {
    mockMvc = MockMvcBuilders.standaloneSetup(brandAuthoriseController).build();
    brandAuthorisationDetailWebResponse =
      brandAuthorisationDetailWebResponse.builder().brandName(BRAND_NAME).sellerName(SELLER_NAME)
        .authStartDate(new Date()).authExpireDate(new Date())
        .authorisationStatus(BrandAuthorisationStatus.ACTIVE).build();
    when(clientParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    when(clientParameterHelper.getStoreId()).thenReturn(Constants.STORE_ID);
    when(clientParameterHelper.getUsername()).thenReturn(Constants.USER_NAME);
    brandAuthCreateWebResponse = new BrandAuthCreateWebResponse();
    brandAuthCreateWebResponse.setBrandCode(BRAND_CODE);

    Date startDate = new Date();
    Calendar calendar = Calendar.getInstance();
    calendar.setTime(startDate);
    calendar.add(Calendar.DATE, 1);
    startDate = calendar.getTime();
    calendar.add(Calendar.DATE, 10);
    Date endDate = calendar.getTime();
    brandAuthCreateWebRequest =
      BrandAuthCreateWebRequest.builder().brandCode(BRAND_CODE).brandName(BRAND_NAME)
        .sellerCode(SELLER_CODE).authStartDate(startDate).authExpireDate(endDate)
        .authorisationStatus("ACTIVE").build();

    brandAuthHistoryRequest = new BrandAuthHistoryRequest();
    brandAuthHistoryWebRequest = new BrandAuthHistoryWebRequest();

    brandAuthHistoryRequest.setBrandCode(BRAND_CODE);
    brandAuthHistoryRequest.setSellerCode(SELLER_CODE);
    brandAuthHistoryWebRequest.setBrandCode(BRAND_CODE);
    brandAuthHistoryWebRequest.setBrandCode(SELLER_CODE);

    brandAuthHistoryResponse = new BrandAuthHistoryResponse();
    brandAuthHistoryResponse.setBrandCode(BRAND_CODE);
    brandAuthHistoryResponse.setSellerCode(SELLER_CODE);
    brandAuthHistoryResponse.setActivity(ACTIVITY_AUTH);
    brandAuthHistoryResponse.setOldStatus(OLD_VALUE_AUTH);
    brandAuthHistoryResponse.setNewStatus(NEW_VALUE_AUTH);
    brandAuthHistoryResponseList.add(brandAuthHistoryResponse);
    historyResponse.setSuccess(true);
    historyResponse.setContent(brandAuthHistoryResponseList);

    fileContent = new byte[]{-1, -40, -20, -10};

    brandAuthDeleteRequestList.add(
      new BrandAuthDeleteRequest(SELLER_CODE, BRAND_CODE, "id", "status"));

  }

  @AfterEach
  public void tearDown() {
    verifyNoMoreInteractions(brandAuthorisationService);
    verifyNoMoreInteractions(clientParameterHelper);
  }

  @Test
  public void getBrandAuthDetailByBrandCodeTest() throws Exception {
    when(
      this.brandAuthorisationService.getBrandAuthDetailBySellerCodeAndBrandCode(anyString(),
        anyString(),anyString())).thenReturn(brandAuthorisationDetailWebResponse);
    MockHttpServletRequestBuilder requestBuilder =
      get(BrandAuthoriseApiPath.BASE_PATH + AUTH_DETAIL_API_PATH).param("sellerCode", SELLER_CODE)
        .param("requestId", Constants.REQUEST_ID).param("storeId", Constants.STORE_ID)
        .contentType(MediaType.APPLICATION_JSON);
    mockMvc.perform(requestBuilder).andExpect(status().isOk())
      .andExpect(jsonPath("$.success", equalTo(true)));
    verify(clientParameterHelper).getRequestId();
    verify(clientParameterHelper).getStoreId();
    verify(brandAuthorisationService).getBrandAuthDetailBySellerCodeAndBrandCode(anyString(),
      anyString(), anyString());
  }

  @Test
  public void getAuthorisationsTest() throws Exception {
    when(this.brandAuthorisationService.getAuthorisations(any(BrandAuthWebRequest.class), Mockito.anyInt(),
        Mockito.anyInt())).thenReturn(new PageImpl<>(new ArrayList<>()));
    String requestString =
        BrandAuthorisationControllerTest.OBJECT_MAPPER.writeValueAsString(new BrandAuthFilterRequest());
    mockMvc.perform(
        post(BrandAuthoriseApiPath.BASE_PATH + BrandAuthoriseApiPath.LISTING)
            .param("page", "0")
            .param("size", "10")
            .contentType(MediaType.APPLICATION_JSON)
            .content(requestString)).andExpect(status().isOk());
    verify(clientParameterHelper).getRequestId();
    verify(brandAuthorisationService).getAuthorisations(any(BrandAuthWebRequest.class), Mockito.anyInt(),
        Mockito.anyInt());
  }

  @Test
  public void getBrandAuthDetailByBrandCodeExceptionTest() throws Exception {
    doThrow(ApplicationRuntimeException.class).when(brandAuthorisationService)
      .getBrandAuthDetailBySellerCodeAndBrandCode(anyString(), anyString(), anyString());
    MockHttpServletRequestBuilder requestBuilder =
      get(BrandAuthoriseApiPath.BASE_PATH + AUTH_DETAIL_API_PATH).param("requestId",
          Constants.REQUEST_ID).param("sellerCode", SELLER_CODE)
        .contentType(MediaType.APPLICATION_JSON);
    mockMvc.perform(requestBuilder).andExpect(jsonPath("$.success", equalTo(Boolean.FALSE)));
    verify(clientParameterHelper).getRequestId();
    verify(clientParameterHelper).getStoreId();

    verify(brandAuthorisationService).getBrandAuthDetailBySellerCodeAndBrandCode(anyString(),
      anyString(), anyString());
  }

  @Test
  public void getBrandAuthDetailByBrandCode_MissingSellerCodeTest() throws Exception {
    doThrow(ApplicationRuntimeException.class).when(brandAuthorisationService)
      .getBrandAuthDetailBySellerCodeAndBrandCode(Constants.STORE_ID, SELLER_CODE, BRAND_CODE);
    MockHttpServletRequestBuilder requestBuilder =
      get(BrandAuthoriseApiPath.BASE_PATH + AUTH_DETAIL_API_PATH).param("requestId",
          Constants.REQUEST_ID).param("storeId", Constants.STORE_ID)
        .contentType(MediaType.APPLICATION_JSON);
    String actions =
      mockMvc.perform(requestBuilder).andReturn().getResponse().getContentAsString();
    Assertions.assertTrue(StringUtils.isAllEmpty(actions));
    verifyNoMoreInteractions(clientParameterHelper,brandAuthorisationService);
  }

  @Test
  public void deleteTest() throws Exception {
    doNothing().when(this.brandAuthorisationService)
      .delete(anyString(), anyString(), any());
    MockHttpServletRequestBuilder requestBuilder =
      post(BrandAuthoriseApiPath.BASE_PATH + DELETE_API_PATH).contentType(MediaType.APPLICATION_JSON).content(objectMapper.writeValueAsString(brandAuthDeleteRequestList));
    mockMvc.perform(requestBuilder).andExpect(status().isOk())
      .andExpect(jsonPath("$.success", equalTo(Boolean.TRUE)));
    verify(brandAuthorisationService).delete(anyString(), anyString(), any());
    verify(clientParameterHelper).getRequestId();
    verify(clientParameterHelper).getStoreId();
    verify(clientParameterHelper).getUsername();
  }
  @Test
  public void createBrandAuthTest() throws Exception {
    ReflectionTestUtils.setField(brandAuthoriseController, "numberOfYears", 5);
    String requestBody =
      this.objectMapper.writeValueAsString(brandAuthCreateWebRequest);
    when(
      this.brandAuthorisationService.create(anyString(),
        anyString(),  anyString(),any(BrandAuthCreateRequest.class))).thenReturn(brandAuthCreateWebResponse);
    URI uri =
      new URIBuilder().setPath(BrandAuthoriseApiPath.BASE_PATH + BrandAuthoriseApiPath.CREATE).build();
    this.mockMvc.perform(
      MockMvcRequestBuilders.post(uri).content(requestBody).contentType(MediaType.APPLICATION_JSON))
      .andExpect(MockMvcResultMatchers.status().isOk())
      .andExpect(jsonPath("$.success", equalTo(true)));
    verify(clientParameterHelper).getRequestId();
    verify(clientParameterHelper).getStoreId();
    verify(clientParameterHelper).getUsername();

    verify(brandAuthorisationService).create(anyString(),
      anyString(),  anyString(),any(BrandAuthCreateRequest.class));
  }

  @Test
  public void createBrandAuthExceptionTest() throws Exception {
    brandAuthCreateWebRequest.setAuthExpireDate(new Date());
    String requestBody =
      this.objectMapper.writeValueAsString(brandAuthCreateWebRequest);
    try {
      URI uri =
        new URIBuilder().setPath(BrandAuthoriseApiPath.BASE_PATH + BrandAuthoriseApiPath.CREATE).build();
      this.mockMvc.perform(MockMvcRequestBuilders.post(uri).content(requestBody).contentType(MediaType.APPLICATION_JSON)).andExpect(MockMvcResultMatchers.status().isOk())
        .andExpect(jsonPath("$.success", equalTo(false)));
    }
    catch (Exception ex){
      Assertions.assertTrue(ex.getMessage().contains(ErrorMessages.AUTH_END_DATE_MUST_NOT_BE_BEFORE_START_DATE));
    }
    finally {
      verify(clientParameterHelper).getRequestId();
      verify(clientParameterHelper).getStoreId();
      verify(clientParameterHelper).getUsername();
    }
  }

  @Test
  public void updateBrandAuthTest() throws Exception {
    String requestBody =
        this.objectMapper.writeValueAsString(brandAuthCreateWebRequest);
    doNothing().when(this.brandAuthorisationService)
        .update(anyString(), anyString(), anyString(),
            any(BrandAuthUpdateRequest.class));
    URI uri =
        new URIBuilder().setPath(BrandAuthoriseApiPath.BASE_PATH + BrandAuthoriseApiPath.UPDATE).build();
    this.mockMvc.perform(MockMvcRequestBuilders.post(uri).content(requestBody).contentType(MediaType.APPLICATION_JSON))
        .andExpect(MockMvcResultMatchers.status().isOk()).andExpect(jsonPath("$.success", equalTo(true)));
    verify(clientParameterHelper).getRequestId();
    verify(clientParameterHelper).getStoreId();
    verify(clientParameterHelper).getUsername();
    verify(brandAuthorisationService)
        .update(anyString(), anyString(), anyString(), any(BrandAuthUpdateRequest.class));
  }

  @Test
  public void deleteExceptionTest() throws Exception {
    doThrow(Exception.class).when(this.brandAuthorisationService).delete(anyString(), anyString(), any());
    MockHttpServletRequestBuilder requestBuilder =
        post(BrandAuthoriseApiPath.BASE_PATH + DELETE_API_PATH).contentType(MediaType.APPLICATION_JSON)
            .content(objectMapper.writeValueAsString(brandAuthDeleteRequestList));
    mockMvc.perform(requestBuilder).andExpect(status().isOk())
      .andExpect(jsonPath("$.success", equalTo(Boolean.FALSE)));
    verify(this.brandAuthorisationService).delete(anyString(), anyString(), any());
    verify(clientParameterHelper).getRequestId();
    verify(clientParameterHelper).getStoreId();
    verify(clientParameterHelper).getUsername();
  }

  @Test
  public void getBrandAuthHistoryTest() throws Exception {
    Mockito.when(this.brandAuthorisationService
      .getBrandAuthHistory(anyString(), anyString(), anyString(),
        any(BrandAuthHistoryRequest.class), Mockito.anyInt(), Mockito.anyInt()))
      .thenReturn(historyResponse);
    this.mockMvc.perform(
      post(BrandAuthoriseApiPath.BASE_PATH + BrandAuthoriseApiPath.HISTORY_SUMMARY)
        .contentType(MediaType.APPLICATION_JSON).content(toJson(brandAuthHistoryRequest))
        .param("page", toJson(PAGE)).param("size", toJson(SIZE))).andExpect(status().isOk());
    Mockito.verify(this.brandAuthorisationService)
      .getBrandAuthHistory(anyString(), anyString(), anyString(),
        any(BrandAuthHistoryRequest.class), Mockito.anyInt(), Mockito.anyInt());
    verify(clientParameterHelper).getRequestId();
    verify(clientParameterHelper).getStoreId();
    verify(clientParameterHelper).getUsername();
  }

  @Test
  public void brandAuthDocUploadTest() throws Exception {
    mockFile(PATH + FILE);
    multipartFile =
      new MockMultipartFile("multipartFile", ORIGINAL_FILENAME, "application/vnd.ms-excel",
        FileUtils.readFileToByteArray(new File(PATH + FILE)));
    when(clientParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    this.mockMvc.perform(MockMvcRequestBuilders.multipart(
          BrandAuthoriseApiPath.BASE_PATH + BrandAuthoriseApiPath.UPLOAD_BRAND_AUTH_DOC)
        .file(multipartFile).param("documentFileName", ORIGINAL_FILENAME)
        .param("brandCode", BRAND_CODE).param("sellerCode", SELLER_CODE)
        .accept(MediaType.APPLICATION_JSON))
      .andExpect(MockMvcResultMatchers.jsonPath("$.success", CoreMatchers.equalTo(true)));
    verify(clientParameterHelper).getRequestId();
    verify(brandAuthorisationService).uploadBrandAuthDoc(ORIGINAL_FILENAME, BRAND_CODE, SELLER_CODE,
      multipartFile.getBytes());
  }

  @Test
  public void brandAuthDocUploadExceptionTest() throws Exception {
    mockFile(PATH + FILE);
    multipartFile =
      new MockMultipartFile("multipartFile", ORIGINAL_FILENAME, "application/vnd.ms-excel",
        FileUtils.readFileToByteArray(new File(PATH + FILE)));
    when(clientParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    Mockito.doThrow(IOException.class).when(brandAuthorisationService)
      .uploadBrandAuthDoc(ORIGINAL_FILENAME, BRAND_CODE, SELLER_CODE, multipartFile.getBytes());
    this.mockMvc.perform(MockMvcRequestBuilders.multipart(
          BrandAuthoriseApiPath.BASE_PATH + BrandAuthoriseApiPath.UPLOAD_BRAND_AUTH_DOC)
        .file(multipartFile).param("documentFileName", ORIGINAL_FILENAME)
        .param("brandCode", BRAND_CODE).param("sellerCode", SELLER_CODE)
        .accept(MediaType.APPLICATION_JSON))
      .andExpect(MockMvcResultMatchers.jsonPath("$.success", CoreMatchers.equalTo(false)));

    verify(clientParameterHelper).getRequestId();
    verify(brandAuthorisationService).uploadBrandAuthDoc(ORIGINAL_FILENAME, BRAND_CODE, SELLER_CODE,
      multipartFile.getBytes());
  }

  @Test
  public void bulkBrandAuthUploadTets() throws Exception {
    mockFile(PATH + FILE);
    multipartFile = new MockMultipartFile("request", ORIGINAL_FILENAME, "application/vnd.ms-excel",
        FileUtils.readFileToByteArray(new File(PATH + FILE)));
    when(clientParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    when(clientParameterHelper.getUsername()).thenReturn(Constants.USER_NAME);
    when(clientParameterHelper.getStoreId()).thenReturn(Constants.STORE_ID);
    this.mockMvc.perform(MockMvcRequestBuilders.multipart(
                BrandAuthoriseApiPath.BASE_PATH + BrandAuthoriseApiPath.BRAND_AUTH_BULK_UPLOAD, BRAND_AUTH_ADD)
            .file(multipartFile).accept(MediaType.APPLICATION_JSON))
        .andExpect(MockMvcResultMatchers.jsonPath("$.success", CoreMatchers.equalTo(true)));
    Mockito.verify(brandAuthorisationService)
        .saveBulkBrandAuthFile(multipartFile, BRAND_AUTH_ADD, Constants.REQUEST_ID, Constants.STORE_ID,
            Constants.USER_NAME);
    verify(clientParameterHelper).getRequestId();
    verify(clientParameterHelper).getStoreId();
    verify(clientParameterHelper).getUsername();
  }

  @Test
  public void bulkBrandAuthUploadTetsInvalidType() throws Exception {
    mockFile(PATH + FILE);
    multipartFile = new MockMultipartFile("request", ORIGINAL_FILENAME, "application/vnd.ms-excel",
        FileUtils.readFileToByteArray(new File(PATH + FILE)));
    when(clientParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    try {
      this.mockMvc.perform(MockMvcRequestBuilders.multipart(
                  BrandAuthoriseApiPath.BASE_PATH + BrandAuthoriseApiPath.BRAND_AUTH_BULK_UPLOAD, REQUEST_ID)
              .file(multipartFile).accept(MediaType.APPLICATION_JSON))
          .andExpect(MockMvcResultMatchers.jsonPath("$.success", CoreMatchers.equalTo(true)));
    } catch (Exception ex){
      Assertions.assertNotNull(ex);
    } finally {
      verify(clientParameterHelper).getRequestId();
    }
  }

  @Test
  public void bulkRestrictedKeywordUpload_ExceptionTest() throws Exception {
    mockFile(PATH + FILE);
    multipartFile = new MockMultipartFile("request", ORIGINAL_FILENAME, "application/vnd.ms-excel",
        FileUtils.readFileToByteArray(new File(PATH + FILE)));
    Mockito.doThrow(IOException.class).when(brandAuthorisationService)
        .saveBulkBrandAuthFile(multipartFile, BRAND_AUTH_DELETE, Constants.REQUEST_ID, Constants.STORE_ID,
            Constants.USER_NAME);
    when(clientParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    when(clientParameterHelper.getUsername()).thenReturn(Constants.USER_NAME);
    when(clientParameterHelper.getStoreId()).thenReturn(Constants.STORE_ID);
    this.mockMvc.perform(MockMvcRequestBuilders.multipart(
                BrandAuthoriseApiPath.BASE_PATH + BrandAuthoriseApiPath.BRAND_AUTH_BULK_UPLOAD, BRAND_AUTH_DELETE)
            .file(multipartFile).accept(MediaType.APPLICATION_JSON))
        .andExpect(MockMvcResultMatchers.jsonPath("$.success", CoreMatchers.equalTo(false)));
    Mockito.verify(brandAuthorisationService)
        .saveBulkBrandAuthFile(multipartFile, BRAND_AUTH_DELETE, Constants.REQUEST_ID, Constants.STORE_ID,
            Constants.USER_NAME);
    verify(clientParameterHelper).getRequestId();
    verify(clientParameterHelper).getUsername();
    verify(clientParameterHelper).getStoreId();
  }

  private void mockFile(String filePath) throws IOException {
    File file = new File(filePath);
    FileUtils.writeByteArrayToFile(file, fileContent);
  }

  @Test
  public void downloadAllBrandAuthsTest() throws Exception {
    Mockito.doNothing().when(this.brandAuthorisationService)
        .brandAuthDownloadAll(Mockito.eq(Constants.USER_NAME), any(BrandAuthWebRequest.class));
    String requestString =
        BrandAuthorisationControllerTest.OBJECT_MAPPER.writeValueAsString(new BrandAuthFilterRequest());
    mockMvc.perform(
        post(BrandAuthoriseApiPath.BASE_PATH + BrandAuthoriseApiPath.BRAND_AUTH_DOWNLOAD_ALL)
            .contentType(MediaType.APPLICATION_JSON).content(requestString)).andExpect(status().isOk());
    verify(clientParameterHelper).getRequestId();
    verify(clientParameterHelper).getUsername();
    Mockito.verify(this.brandAuthorisationService)
        .brandAuthDownloadAll(Mockito.eq(Constants.USER_NAME), any(BrandAuthWebRequest.class));
  }
}
