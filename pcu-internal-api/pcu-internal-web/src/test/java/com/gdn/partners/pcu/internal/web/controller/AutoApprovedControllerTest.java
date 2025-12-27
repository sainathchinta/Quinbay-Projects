package com.gdn.partners.pcu.internal.web.controller;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.PageMetaData;
import com.gdn.partners.core.web.dto.SingleBaseResponse;
import com.gdn.partners.pcu.internal.client.helper.ClientParameterHelper;
import com.gdn.partners.pcu.internal.client.model.request.AutoApprovedAssigneeRequest;
import com.gdn.partners.pcu.internal.client.model.request.AutoApprovedWebRequest;
import com.gdn.partners.pcu.internal.client.model.request.AutoApprovedUserFeedbackRequest;
import com.gdn.partners.pcu.internal.client.model.response.AutoApprovedListWebResponse;
import com.gdn.partners.pcu.internal.client.model.response.ProductAssigneeChangeResponse;
import com.gdn.partners.pcu.internal.model.AutoApprovedApiPath;
import com.gdn.partners.pcu.internal.model.Constants;
import com.gdn.partners.pcu.internal.service.AutoApprovedService;
import com.gdn.partners.pcu.internal.service.AutoApprovedServiceWrapper;
import com.gdn.partners.pcu.internal.web.helper.TestHelper;
import com.gdn.partners.pcu.internal.web.model.request.AutoApprovedProductsActionWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.AutoApprovedProductsDownloadWebRequest;
import com.gdn.partners.pcu.internal.web.model.response.ProductDetailWebResponse;
import org.apache.commons.io.FileUtils;
import org.hamcrest.CoreMatchers;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.Assertions;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.http.MediaType;
import org.springframework.mock.web.MockMultipartFile;
import org.springframework.test.web.servlet.request.MockHttpServletRequestBuilder;
import org.springframework.test.web.servlet.request.MockMvcRequestBuilders;
import org.springframework.test.web.servlet.result.MockMvcResultMatchers;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;

import static org.hamcrest.Matchers.equalTo;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

@AutoConfigureMockMvc
public class AutoApprovedControllerTest extends TestHelper {

  private static final int PAGE = 0;
  private static final int SIZE = 10;
  private static final int TOTAL_SIZE = 0;
  private static final String PRODUCT_CODE = "product-code";
  private static final String REQUEST_ID = "requestId";
  private static final String CLIENT_ID = "clientId";
  private static final String STORE_ID = "store-id";
  private static final String CHANNEL_ID = "channel-id";
  private static final String USERNAME = "username";
  private static final String FILE = "/filestore/originalFilename.xls";
  private static final String PATH = "path";
  private static final String ORIGINAL_FILENAME = "originalFilename.xls";
  private byte[] fileContent;

  @InjectMocks
  private AutoApprovedController autoApprovedController;

  @Mock
  private AutoApprovedService autoApprovedService;

  @Mock
  private AutoApprovedServiceWrapper autoApprovedServiceWrapper;

  @Mock
  private ClientParameterHelper clientParameterHelper;

  private ObjectMapper objectMapper;

  @BeforeEach
  public void init() {
    MockitoAnnotations.initMocks(this);
    mockMvc = MockMvcBuilders.standaloneSetup(this.autoApprovedController).build();
    objectMapper = new ObjectMapper();
    fileContent = new byte[] {-1, -40, -20, -10};
  }

  @AfterEach
  public void teardown() {
    Mockito.verifyNoMoreInteractions(autoApprovedService);
    Mockito.verifyNoMoreInteractions(clientParameterHelper);
    Mockito.verifyNoMoreInteractions(autoApprovedServiceWrapper);
  }

  @Test
  public void getAutoApprovedProductsTest() throws Exception {
    AutoApprovedWebRequest request = new AutoApprovedWebRequest();
    GdnRestListResponse<AutoApprovedListWebResponse> serviceResponse = new GdnRestListResponse<>(
        new ArrayList<>(), new PageMetaData(10, 0, 10), Constants.REQUEST_ID);
    Mockito.when(autoApprovedService.getAutoApprovedProductsList(PAGE, SIZE, request))
        .thenReturn(serviceResponse);
    Mockito.when(clientParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);

    MockHttpServletRequestBuilder requestBuilder =
        post(AutoApprovedApiPath.BASE_PATH + AutoApprovedApiPath.AUTO_APPROVED_PRODUCTS_PATH)
            .contentType(MediaType.APPLICATION_JSON_VALUE)
            .content(new ObjectMapper().writeValueAsString(request))
            .param("page", String.valueOf(PAGE)).param("size", String.valueOf(SIZE));

    mockMvc.perform(requestBuilder).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)));

    Mockito.verify(autoApprovedService).getAutoApprovedProductsList(PAGE, SIZE, request);
    Mockito.verify(clientParameterHelper).getRequestId();
    Mockito.verifyNoMoreInteractions(autoApprovedService);
    Mockito.verifyNoMoreInteractions(clientParameterHelper);
  }

  @Test
  public void updateAcceptRejectAnchors() throws Exception {
    AutoApprovedProductsActionWebRequest autoApprovedProductsActionWebRequest =
        new AutoApprovedProductsActionWebRequest();
    GdnBaseRestResponse serviceResponse =
        new GdnBaseRestResponse(null, null, true, Constants.REQUEST_ID);
    MockHttpServletRequestBuilder requestBuilder = post(AutoApprovedApiPath.BASE_PATH
        + AutoApprovedApiPath.PERFORM_ACTION_ON_AUTO_APPROVED_PRODUCTS, PRODUCT_CODE)
            .contentType(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON_VALUE)
            .content(new ObjectMapper().writeValueAsString(autoApprovedProductsActionWebRequest))
            .accept(MediaType.APPLICATION_JSON);
    mockMvc.perform(requestBuilder).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)));
    Mockito.verify(autoApprovedServiceWrapper).performActionOnAutoApprovedProducts(PRODUCT_CODE,
        autoApprovedProductsActionWebRequest);
    Mockito.verify(clientParameterHelper).getRequestId();
  }

  @Test
  public void testGetProductDetail_Success() {
    Mockito.when(clientParameterHelper.getRequestId()).thenReturn(REQUEST_ID);
    Mockito.when(clientParameterHelper.getClientId()).thenReturn(CLIENT_ID);
    ProductDetailWebResponse mockResponse = new ProductDetailWebResponse();
    Mockito.when(autoApprovedServiceWrapper.fetchAutoApprovedProductDetail(PRODUCT_CODE, CLIENT_ID))
        .thenReturn(mockResponse);
    SingleBaseResponse<ProductDetailWebResponse> result =
        autoApprovedController.getProductDetail(PRODUCT_CODE);
    Assertions.assertNotNull(result);
    Assertions.assertTrue(result.isSuccess());
    Assertions.assertEquals(REQUEST_ID, result.getRequestId());
    Assertions.assertEquals(mockResponse, result.getValue());
    Mockito.verify(clientParameterHelper).getRequestId();
    Mockito.verify(clientParameterHelper).getClientId();
    Mockito.verify(autoApprovedServiceWrapper).fetchAutoApprovedProductDetail(PRODUCT_CODE, CLIENT_ID);
  }

  @Test
  public void updateAssigneeTest() throws Exception {
    AutoApprovedAssigneeRequest assigneeRequest = new AutoApprovedAssigneeRequest();
    GdnRestListResponse<ProductAssigneeChangeResponse> serviceResponse = new GdnRestListResponse<>(
      new ArrayList<>(), null, Constants.REQUEST_ID);
    Mockito.when(autoApprovedService.updateAssignee(assigneeRequest))
      .thenReturn(serviceResponse);
    this.mockMvc.perform(
      post(AutoApprovedApiPath.BASE_PATH + AutoApprovedApiPath.UPDATE_ASSIGNEE).contentType(
          MediaType.APPLICATION_JSON).content(objectMapper.writeValueAsString(assigneeRequest))
        .accept(MediaType.APPLICATION_JSON).param("storeId", STORE_ID)
        .param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID).param("requestId", REQUEST_ID)
        .param("username", USERNAME)).andExpect(status().isOk());
    Mockito.verify(autoApprovedService).updateAssignee(assigneeRequest);
    Mockito.verify(clientParameterHelper).getRequestId();
  }

  @Test
  public void updateUserFeedbackTest() throws Exception {
    AutoApprovedUserFeedbackRequest autoApprovedUserFeedbackRequest = new AutoApprovedUserFeedbackRequest();
    this.mockMvc.perform(
            post(AutoApprovedApiPath.BASE_PATH + AutoApprovedApiPath.UPDATE_USER_FEEDBACK, PRODUCT_CODE).contentType(
                    MediaType.APPLICATION_JSON).content(objectMapper.writeValueAsString(autoApprovedUserFeedbackRequest))
                .accept(MediaType.APPLICATION_JSON).param("storeId", STORE_ID).param("channelId", CHANNEL_ID)
                .param("clientId", CLIENT_ID).param("requestId", REQUEST_ID).param("username", USERNAME))
        .andExpect(status().isOk());
    Mockito.verify(autoApprovedService).updateUserFeedback(PRODUCT_CODE, autoApprovedUserFeedbackRequest);
  }

  @Test
  public void fetchUserFeedbackTest() throws Exception {
    this.mockMvc.perform(
        get(AutoApprovedApiPath.BASE_PATH + AutoApprovedApiPath.FETCH_USER_FEEDBACK, PRODUCT_CODE).contentType(
                MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON).param("storeId", STORE_ID)
            .param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID).param("requestId", REQUEST_ID)
            .param("username", USERNAME)).andExpect(status().isOk());
    Mockito.verify(autoApprovedService).fetchUserFeedback(PRODUCT_CODE);
  }

  @Test
  public void downloadItemsForAutoApprovedProductsTest() throws Exception {
    AutoApprovedProductsDownloadWebRequest autoApprovedProductsDownloadWebRequest =
        new AutoApprovedProductsDownloadWebRequest();
    when(clientParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    when(clientParameterHelper.getUsername()).thenReturn(Constants.USER_NAME);
    MockHttpServletRequestBuilder requestBuilder = post(
        AutoApprovedApiPath.BASE_PATH + AutoApprovedApiPath.AUTO_APPROVED_PRODUCT_DOWNLOAD).content(
            toJson(autoApprovedProductsDownloadWebRequest)).contentType(MediaType.APPLICATION_JSON)
        .accept(MediaType.APPLICATION_JSON).param("storeId", STORE_ID)
        .param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID).param("requestId", REQUEST_ID)
        .param("username", USERNAME);
    mockMvc.perform(requestBuilder).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)));
    Mockito.verify(autoApprovedServiceWrapper)
        .downloadItemsForAutoApprovedProducts(USERNAME, autoApprovedProductsDownloadWebRequest);
    verify(clientParameterHelper).getRequestId();
    verify(clientParameterHelper).getUsername();
  }

  @Test
  public void bulkUploadFileAssigneeTest() throws Exception {
    mockFile(PATH + FILE);
    MockMultipartFile file = new MockMultipartFile("file", ORIGINAL_FILENAME,
        "application/vnd" + ".ms-excel", FileUtils.readFileToByteArray(new File(PATH + FILE)));
    when(clientParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    when(clientParameterHelper.getUsername()).thenReturn(Constants.USER_NAME);
    when(clientParameterHelper.getStoreId()).thenReturn(Constants.STORE_ID);
    when(clientParameterHelper.getVendorCode()).thenReturn(Constants.VENDOR_CODE);

    this.mockMvc
        .perform(MockMvcRequestBuilders
            .multipart(AutoApprovedApiPath.BASE_PATH
                + AutoApprovedApiPath.AUTO_APPROVED_PRODUCT_UPLOAD_ASSIGNEE)
            .file(file).accept(MediaType.APPLICATION_JSON))
        .andExpect(MockMvcResultMatchers.jsonPath("$.success", CoreMatchers.equalTo(true)));

    Mockito.verify(autoApprovedServiceWrapper).uploadBulkAssignFile(file, Constants.REQUEST_ID,
        Constants.STORE_ID, Constants.USER_NAME, Constants.VENDOR_CODE);
    verify(clientParameterHelper).getRequestId();
    verify(clientParameterHelper).getStoreId();
    verify(clientParameterHelper).getUsername();
    verify(clientParameterHelper).getVendorCode();
  }

  @Test
  public void bulkUploadFileExceptionTest() throws Exception {
    mockFile(PATH + FILE);
    MockMultipartFile multipartFile = new MockMultipartFile("file", ORIGINAL_FILENAME,
        "application/vnd.ms-excel", FileUtils.readFileToByteArray(new File(PATH + FILE)));
    Mockito.doThrow(IOException.class).when(autoApprovedServiceWrapper)
        .uploadBulkAssignFile(multipartFile, Constants.REQUEST_ID, Constants.STORE_ID, USERNAME,
          Constants.VENDOR_CODE);
    when(clientParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    when(clientParameterHelper.getUsername()).thenReturn(Constants.USER_NAME);
    when(clientParameterHelper.getStoreId()).thenReturn(Constants.STORE_ID);
    when(clientParameterHelper.getVendorCode()).thenReturn(Constants.VENDOR_CODE);
    this.mockMvc
        .perform(MockMvcRequestBuilders
            .multipart(AutoApprovedApiPath.BASE_PATH
                + AutoApprovedApiPath.AUTO_APPROVED_PRODUCT_UPLOAD_ASSIGNEE)
            .file(multipartFile).accept(MediaType.APPLICATION_JSON))
        .andExpect(MockMvcResultMatchers.jsonPath("$.success", CoreMatchers.equalTo(false)));
    Mockito.verify(autoApprovedServiceWrapper).uploadBulkAssignFile(multipartFile,
        Constants.REQUEST_ID, Constants.STORE_ID, USERNAME, Constants.VENDOR_CODE);
    verify(clientParameterHelper).getRequestId();
    verify(clientParameterHelper).getUsername();
    verify(clientParameterHelper).getStoreId();
    verify(clientParameterHelper).getVendorCode();
  }

  private void mockFile(String filePath) throws IOException {
    File file = new File(filePath);
    FileUtils.writeByteArrayToFile(file, fileContent);
  }
}
