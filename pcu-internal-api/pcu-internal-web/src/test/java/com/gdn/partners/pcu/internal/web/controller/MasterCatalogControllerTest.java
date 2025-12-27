package com.gdn.partners.pcu.internal.web.controller;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.common.web.wrapper.response.PageMetaData;
import com.gdn.partners.pcu.internal.client.helper.ClientParameterHelper;
import com.gdn.partners.pcu.internal.client.model.request.ChangeAssigneeRequest;
import com.gdn.partners.pcu.internal.client.model.request.ClusterReviewFeedbackRequest;
import com.gdn.partners.pcu.internal.client.model.request.AcceptRejectActionRequest;
import com.gdn.partners.pcu.internal.client.model.request.ItemSkuListRequest;
import com.gdn.partners.pcu.internal.client.model.request.MasterSkuItemsListWebRequest;
import com.gdn.partners.pcu.internal.client.model.response.ClusterActionResponse;
import com.gdn.partners.pcu.internal.client.model.response.CompareAnchorResponse;
import com.gdn.partners.pcu.internal.client.model.response.ItemHistoryResponse;
import com.gdn.partners.pcu.internal.client.model.response.ItemSkuAndIndicativePriceResponse;
import com.gdn.partners.pcu.internal.client.model.response.ItemSkuDetailResponse;
import com.gdn.partners.pcu.internal.client.model.response.ItemsListingResponse;
import com.gdn.partners.pcu.internal.client.model.response.MasterSkuConfigResponse;
import com.gdn.partners.pcu.internal.model.Constants;
import com.gdn.partners.pcu.internal.model.ErrorMessages;
import com.gdn.partners.pcu.internal.model.MasterCatalogApiPath;
import com.gdn.partners.pcu.internal.service.impl.MasterSkuReviewServiceImpl;
import com.gdn.partners.pcu.internal.service.model.BulkInternalProcessType;
import com.gdn.partners.pcu.internal.web.helper.TestHelper;
import com.gdn.partners.pcu.internal.client.model.request.InReviewListWebRequest;
import com.gdn.partners.pcu.internal.client.model.response.InReviewListResponse;
import com.gdn.partners.pcu.internal.web.model.request.MasterSkuItemsDownloadWebRequest;

import com.gdn.partners.pcu.internal.web.model.request.MasterSkuInReviewDownloadWebRequest;
import org.apache.commons.io.FileUtils;
import org.apache.commons.lang3.StringUtils;
import org.hamcrest.CoreMatchers;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
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

import static org.hamcrest.Matchers.equalTo;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.put;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;

@AutoConfigureMockMvc
public class MasterCatalogControllerTest extends TestHelper {

  private static final String MASTER_SKU = "master-sku";
  private static final String USER_NAME = "username";
  private static final int PAGE = 0;
  private static final int SIZE = 10;
  private static final int TOTAL_SIZE = 0;
  private static final String FILE = "/filestore/originalFilename.xls";
  private static final String PATH = "path";
  private static final String ORIGINAL_FILENAME = "originalFilename.xls";
  private byte[] fileContent;


  @InjectMocks
  private MasterCatalogController masterCatalogController;

  @Mock
  private MasterSkuReviewServiceImpl masterSkuReviewService;

  @Mock
  private ClientParameterHelper clientParameterHelper;

  @BeforeEach
  public void init() {
    MockitoAnnotations.initMocks(this);
    mockMvc = MockMvcBuilders.standaloneSetup(this.masterCatalogController).build();
    fileContent = new byte[] {-1, -40, -20, -10};
  }

  @AfterEach
  public void teardown() {
    Mockito.verifyNoMoreInteractions(masterSkuReviewService);
    Mockito.verifyNoMoreInteractions(clientParameterHelper);
  }

  @Test
  public void getInReviewProductsTest() throws Exception {
    InReviewListWebRequest request = new InReviewListWebRequest();
    GdnRestListResponse<InReviewListResponse> serviceResponse =
      new GdnRestListResponse<>(new ArrayList<>(), new PageMetaData(10, 0, 10), Constants.REQUEST_ID);


    Mockito.when(masterSkuReviewService.getInReviewProductList(PAGE, SIZE, request))
        .thenReturn(serviceResponse);
    Mockito.when(clientParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);

    MockHttpServletRequestBuilder requestBuilder =
        post(MasterCatalogApiPath.BASE_PATH + MasterCatalogApiPath.IN_REVIEW_PATH)
            .contentType(MediaType.APPLICATION_JSON_VALUE)
            .content(new ObjectMapper().writeValueAsString(request))
            .param("page", String.valueOf(PAGE)).param("size", String.valueOf(SIZE));

    mockMvc.perform(requestBuilder).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)));

    Mockito.verify(masterSkuReviewService).getInReviewProductList(PAGE, SIZE, request);
    Mockito.verify(clientParameterHelper).getRequestId();
    Mockito.verifyNoMoreInteractions(masterSkuReviewService);
    Mockito.verifyNoMoreInteractions(clientParameterHelper);
  }

  @Test
  public void getAllMasterSkuItemsListTest() throws Exception {
    MasterSkuItemsListWebRequest request = new MasterSkuItemsListWebRequest();
    GdnRestListResponse<ItemsListingResponse> serviceResponse =
        new GdnRestListResponse<>(new ArrayList<>(), new PageMetaData(10, 0, 10), Constants.REQUEST_ID);

    Mockito.when(masterSkuReviewService.getAllMasterSkuItemsList(PAGE, SIZE, request)).thenReturn(serviceResponse);
    Mockito.when(clientParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);

    MockHttpServletRequestBuilder requestBuilder =
        post(MasterCatalogApiPath.BASE_PATH + MasterCatalogApiPath.GET_ALL_ITEMS).contentType(
                MediaType.APPLICATION_JSON_VALUE).content(new ObjectMapper().writeValueAsString(request))
            .param("page", String.valueOf(PAGE)).param("size", String.valueOf(SIZE));

    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)));

    Mockito.verify(masterSkuReviewService).getAllMasterSkuItemsList(PAGE, SIZE, request);
    Mockito.verify(clientParameterHelper).getRequestId();
    Mockito.verifyNoMoreInteractions(masterSkuReviewService);
    Mockito.verifyNoMoreInteractions(clientParameterHelper);
  }

  @Test
  public void getMasterSkuDetailsTest() throws Exception {
    ItemSkuDetailResponse itemSkuDetailResponse = new ItemSkuDetailResponse();
    GdnRestSingleResponse<ItemSkuDetailResponse> response =
        new GdnRestSingleResponse<>(itemSkuDetailResponse, Constants.REQUEST_ID);
    when(clientParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    when(masterSkuReviewService.getMasterSkuDetails(MASTER_SKU, true)).thenReturn(response);
    MockHttpServletRequestBuilder requestBuilder =
        get(MasterCatalogApiPath.BASE_PATH + MasterCatalogApiPath.MASTER_SKU_DETAILS, MASTER_SKU).contentType(
            MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON).param("allData", "true");
    mockMvc.perform(requestBuilder).andExpect(status().isOk())
      .andExpect(jsonPath("$.success", equalTo(true)));
    verify(masterSkuReviewService).getMasterSkuDetails(MASTER_SKU, true);
    verify(clientParameterHelper).getRequestId();
  }

  @Test
  public void testGetCompareAnchorDetailsSuccessTest() throws Exception {
    String firstAnchor = "anchor1";
    String secondAnchor = "anchor2";
    CompareAnchorResponse mockResponse = new CompareAnchorResponse();
    GdnRestSingleResponse<CompareAnchorResponse> response =
      new GdnRestSingleResponse<>(mockResponse, Constants.REQUEST_ID);
    Mockito.when(masterSkuReviewService.getCompareAnchorDetails(firstAnchor, secondAnchor))
      .thenReturn(response);
    mockMvc = MockMvcBuilders.standaloneSetup(masterCatalogController).build();
    this.mockMvc.perform(
        get(MasterCatalogApiPath.BASE_PATH + MasterCatalogApiPath.COMPARE_ANCHORS, firstAnchor,
          secondAnchor).contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
      .andExpect(status().isOk()).andReturn();
    Mockito.verify(masterSkuReviewService).getCompareAnchorDetails(firstAnchor, secondAnchor);
    Mockito.verify(clientParameterHelper).getRequestId();
    Mockito.verifyNoMoreInteractions(masterSkuReviewService);
    Mockito.verifyNoMoreInteractions(clientParameterHelper);
  }

  @Test
  public void testGetCompareAnchorDetailsFailureTest() throws Exception {
    String firstAnchor = "anchor1";
    String secondAnchor = "anchor2";
    Mockito.when(masterSkuReviewService.getCompareAnchorDetails(firstAnchor, secondAnchor))
      .thenReturn(
        new GdnRestSingleResponse<CompareAnchorResponse>("Error message", "ErrorCode", false, null,
          null));
    mockMvc = MockMvcBuilders.standaloneSetup(masterCatalogController).build();
    this.mockMvc.perform(
        get(MasterCatalogApiPath.BASE_PATH + MasterCatalogApiPath.COMPARE_ANCHORS, firstAnchor,
          secondAnchor).contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
      .andExpect(status().isOk()).andReturn();
    Mockito.verify(masterSkuReviewService).getCompareAnchorDetails(firstAnchor, secondAnchor);
    Mockito.verify(clientParameterHelper).getRequestId();
    Mockito.verifyNoMoreInteractions(masterSkuReviewService);
    Mockito.verifyNoMoreInteractions(clientParameterHelper);
  }

  @Test
  public void fetchItemsMappedToMasterSku() throws Exception {
    GdnRestListResponse<ItemSkuDetailResponse> serviceResponse =
        new GdnRestListResponse<>(new ArrayList<>(), new PageMetaData(10, 0, 10), Constants.REQUEST_ID);

    Mockito.when(masterSkuReviewService.fetchItemsMappedToMasterSku(PAGE, SIZE, MASTER_SKU))
        .thenReturn(serviceResponse);

    MockHttpServletRequestBuilder requestBuilder =
        get(MasterCatalogApiPath.BASE_PATH + MasterCatalogApiPath.FETCH_ITEMS_MAPPED_TO_MASTER_SKU, MASTER_SKU).contentType(
                MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON).param("page", String.valueOf(PAGE))
            .param("size", String.valueOf(SIZE));

    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)));

    Mockito.verify(masterSkuReviewService).fetchItemsMappedToMasterSku(PAGE, SIZE, MASTER_SKU);
    Mockito.verifyNoMoreInteractions(masterSkuReviewService);
    Mockito.verifyNoMoreInteractions(clientParameterHelper);
  }

  @Test
  public void performClusterActionTest() throws Exception {
    GdnRestSingleResponse<ClusterActionResponse> serviceResponse =
      new GdnRestSingleResponse<ClusterActionResponse>();
    serviceResponse.setSuccess(true);
    ClusterReviewFeedbackRequest request = new ClusterReviewFeedbackRequest();
    Mockito.when(masterSkuReviewService.performClusterAction(MASTER_SKU, request)).thenReturn(serviceResponse);
    MockHttpServletRequestBuilder requestBuilder =
        put(MasterCatalogApiPath.BASE_PATH + MasterCatalogApiPath.PERFORM_CLUSTER_REVIEW_ACTION,
            MASTER_SKU).contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON)
            .content(new ObjectMapper().writeValueAsString(request));
    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)));
    Mockito.verify(masterSkuReviewService).performClusterAction(MASTER_SKU, request);
  }

  @Test
  public void fetchItemHistoryDetailsTest() throws Exception {
    GdnRestListResponse<ItemHistoryResponse> serviceResponse =
        new GdnRestListResponse<>(new ArrayList<>(), new PageMetaData(SIZE, PAGE, 10), Constants.REQUEST_ID);
    Mockito.when(masterSkuReviewService.fetchItemHistoryDetails(PAGE, SIZE, MASTER_SKU)).thenReturn(serviceResponse);
    MockHttpServletRequestBuilder requestBuilder =
        get(MasterCatalogApiPath.BASE_PATH + MasterCatalogApiPath.GET_HISTORY_DETAILS_FOR_ITEM, MASTER_SKU).contentType(
            MediaType.APPLICATION_JSON_VALUE).param("page", String.valueOf(PAGE)).param("size", String.valueOf(SIZE));
    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)));
    Mockito.verify(masterSkuReviewService).fetchItemHistoryDetails(PAGE, SIZE, MASTER_SKU);
    Mockito.verifyNoMoreInteractions(masterSkuReviewService);
  }

  @Test
  public void fetchMasterSkuReviewConfig() throws Exception {
    Mockito.when(masterSkuReviewService.fetchMasterSkuReviewConfig()).thenReturn(
        new GdnRestSingleResponse<MasterSkuConfigResponse>("Error message", "ErrorCode", false, null, null));
    mockMvc = MockMvcBuilders.standaloneSetup(masterCatalogController).build();
    this.mockMvc.perform(
        get(MasterCatalogApiPath.BASE_PATH + MasterCatalogApiPath.GET_MASTER_SKU_REVIEW_CONFIG).contentType(
            MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON)).andExpect(status().isOk()).andReturn();
    Mockito.verify(masterSkuReviewService).fetchMasterSkuReviewConfig();
    Mockito.verifyNoMoreInteractions(masterSkuReviewService);
  }

  @Test
  public void updateAcceptRejectAnchors() throws Exception {
    String firstAnchor = "anchor1";
    String secondAnchor = "anchor2";
    AcceptRejectActionRequest acceptRejectActionRequest = new AcceptRejectActionRequest();
    GdnBaseRestResponse serviceResponse =
        new GdnBaseRestResponse(null, null, true, Constants.REQUEST_ID);
    MockHttpServletRequestBuilder requestBuilder =
        post(MasterCatalogApiPath.BASE_PATH + MasterCatalogApiPath.ACCEPT_REJECT_ACTION, firstAnchor,
            secondAnchor).contentType(MediaType.APPLICATION_JSON)
                .contentType(MediaType.APPLICATION_JSON_VALUE)
                .content(new ObjectMapper().writeValueAsString(acceptRejectActionRequest))
                .accept(MediaType.APPLICATION_JSON);
    mockMvc.perform(requestBuilder).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)));
    Mockito.verify(masterSkuReviewService).updateAcceptRejectActionRequest(firstAnchor,
        secondAnchor, acceptRejectActionRequest);
    Mockito.verify(clientParameterHelper).getRequestId();
    Mockito.verify(clientParameterHelper).getUsername();
  }

  @Test
  public void updateAssigneeChanged() throws Exception {
    ChangeAssigneeRequest changeAssigneeRequest = new ChangeAssigneeRequest();
    MockHttpServletRequestBuilder requestBuilder =
        post(MasterCatalogApiPath.BASE_PATH + MasterCatalogApiPath.CHANGE_ASSIGNEE)
            .contentType(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON_VALUE)
            .content(new ObjectMapper().writeValueAsString(changeAssigneeRequest))
            .accept(MediaType.APPLICATION_JSON);
    mockMvc.perform(requestBuilder).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)));
    Mockito.verify(masterSkuReviewService).updateChangeAssigneeAction(changeAssigneeRequest);
    Mockito.verify(clientParameterHelper).getRequestId();
  }

  @Test
  public void getMasterReviewerListTest() throws Exception {
    Mockito.when(clientParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    Mockito.when(masterSkuReviewService.getMasterSkuReviewReviewers())
      .thenReturn(new ArrayList<>());
    MockHttpServletRequestBuilder requestBuilder =
      get(MasterCatalogApiPath.BASE_PATH + MasterCatalogApiPath.REVIEWERS).contentType(
        MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON);
    mockMvc.perform(requestBuilder).andExpect(status().isOk())
      .andExpect(jsonPath("$.success", equalTo(true)));
    Mockito.verify(masterSkuReviewService).getMasterSkuReviewReviewers();
    verify(this.clientParameterHelper).getRequestId();
  }

  @Test
  public void downloadAllMasterProductsTest() throws Exception {
    MasterSkuItemsDownloadWebRequest masterSkuItemsDownloadWebRequest = new MasterSkuItemsDownloadWebRequest();
    when(clientParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    when(clientParameterHelper.getUsername()).thenReturn(Constants.USER_NAME);
    MockHttpServletRequestBuilder requestBuilder =
        post(MasterCatalogApiPath.BASE_PATH + MasterCatalogApiPath.MASTER_SKU_ITEMS_DOWNLOAD).content(
                toJson(masterSkuItemsDownloadWebRequest)).contentType(MediaType.APPLICATION_JSON)
            .accept(MediaType.APPLICATION_JSON);
    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)));
    Mockito.verify(this.masterSkuReviewService)
        .downloadItemsForMasterSkuReview(USER_NAME, masterSkuItemsDownloadWebRequest, PAGE, TOTAL_SIZE);
    verify(clientParameterHelper).getRequestId();
    verify(clientParameterHelper).getUsername();
  }

  @Test
  public void bulkDownloadInReviewAnchors() throws Exception {
    Mockito.when(clientParameterHelper.getUsername()).thenReturn(USER_NAME);
    MasterSkuInReviewDownloadWebRequest masterSkuInReviewDownloadWebRequest = new MasterSkuInReviewDownloadWebRequest();
    MockHttpServletRequestBuilder requestBuilder =
      post(MasterCatalogApiPath.BASE_PATH + MasterCatalogApiPath.BULK_DOWNLOAD_IN_REVIEW)
        .contentType(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON_VALUE)
        .content(new ObjectMapper().writeValueAsString(masterSkuInReviewDownloadWebRequest))
        .accept(MediaType.APPLICATION_JSON);
    mockMvc.perform(requestBuilder).andExpect(status().isOk())
      .andExpect(jsonPath("$.success", equalTo(true)));
    Mockito.verify(masterSkuReviewService).bulkDownloadInReviewAnchorMappings(USER_NAME,
      masterSkuInReviewDownloadWebRequest, PAGE, TOTAL_SIZE);
    Mockito.verify(clientParameterHelper).getRequestId();
    Mockito.verify(clientParameterHelper).getUsername();
  }

  @Test
  public void bulkUploadFileAssigneeTest() throws Exception {
    mockFile(PATH + FILE);
    MockMultipartFile file =
      new MockMultipartFile("file", ORIGINAL_FILENAME, "application/vnd" + ".ms-excel",
        FileUtils.readFileToByteArray(new File(PATH + FILE)));
    when(clientParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    when(clientParameterHelper.getUsername()).thenReturn(Constants.USER_NAME);
    when(clientParameterHelper.getStoreId()).thenReturn(Constants.STORE_ID);

    this.mockMvc.perform(MockMvcRequestBuilders.multipart(
          MasterCatalogApiPath.BASE_PATH + MasterCatalogApiPath.BULK_UPLOAD).file(file)
        .accept(MediaType.APPLICATION_JSON)
        .param("actionType", BulkInternalProcessType.MASTER_SKU_BULK_ASSIGNEE.name()))
      .andExpect(MockMvcResultMatchers.jsonPath("$.success", CoreMatchers.equalTo(true)));

    Mockito.verify(masterSkuReviewService)
      .uploadBulkFile(file, BulkInternalProcessType.MASTER_SKU_BULK_ASSIGNEE.name(),
        Constants.REQUEST_ID, Constants.STORE_ID, Constants.USER_NAME);
    verify(clientParameterHelper).getRequestId();
    verify(clientParameterHelper).getStoreId();
    verify(clientParameterHelper).getUsername();
  }

  @Test
  public void bulkUploadFileInvalidTypeTest() throws Exception {
    mockFile(PATH + FILE);
    MockMultipartFile file =
      new MockMultipartFile("file", ORIGINAL_FILENAME, "application/vnd" + ".ms-excel",
        FileUtils.readFileToByteArray(new File(PATH + FILE)));
    when(clientParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    try {
      this.mockMvc.perform(MockMvcRequestBuilders.multipart(
            MasterCatalogApiPath.BASE_PATH + MasterCatalogApiPath.BULK_UPLOAD).file(file)
          .accept(MediaType.APPLICATION_JSON).param("actionType", "invalid"))
        .andExpect(MockMvcResultMatchers.jsonPath("$.success", CoreMatchers.equalTo(true)));
    } catch (Exception ex) {
      Assertions.assertTrue(StringUtils.contains(ex.toString(), ErrorMessages.INVALID_ACTION_TYPE));
    } finally {
      verify(clientParameterHelper).getRequestId();
    }
  }

  @Test
  public void bulkUploadFileExceptionTest() throws Exception {
    mockFile(PATH + FILE);
    MockMultipartFile multipartFile =
      new MockMultipartFile("file", ORIGINAL_FILENAME, "application/vnd.ms-excel",
        FileUtils.readFileToByteArray(new File(PATH + FILE)));
    Mockito.doThrow(IOException.class).when(masterSkuReviewService)
      .uploadBulkFile(multipartFile, BulkInternalProcessType.MASTER_SKU_BULK_REVIEW.name(),
        Constants.REQUEST_ID, Constants.STORE_ID, USER_NAME);
    when(clientParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    when(clientParameterHelper.getUsername()).thenReturn(Constants.USER_NAME);
    when(clientParameterHelper.getStoreId()).thenReturn(Constants.STORE_ID);
    this.mockMvc.perform(MockMvcRequestBuilders.multipart(
          MasterCatalogApiPath.BASE_PATH + MasterCatalogApiPath.BULK_UPLOAD).file(multipartFile)
        .accept(MediaType.APPLICATION_JSON)
        .param("actionType", BulkInternalProcessType.MASTER_SKU_BULK_REVIEW.name()))
      .andExpect(MockMvcResultMatchers.jsonPath("$.success", CoreMatchers.equalTo(false)));
    Mockito.verify(masterSkuReviewService)
      .uploadBulkFile(multipartFile, BulkInternalProcessType.MASTER_SKU_BULK_REVIEW.name(),
        Constants.REQUEST_ID, Constants.STORE_ID, USER_NAME);
    verify(clientParameterHelper).getRequestId();
    verify(clientParameterHelper).getUsername();
    verify(clientParameterHelper).getStoreId();
  }

  @Test
  public void fetchIndicativePriceTest() throws Exception {
    ItemSkuListRequest request = new ItemSkuListRequest();
    ItemSkuAndIndicativePriceResponse response = new ItemSkuAndIndicativePriceResponse();
    Mockito.when(masterSkuReviewService.fetchIndicativePrice(request)).thenReturn(response);
    when(clientParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    MockHttpServletRequestBuilder requestBuilder = post(
      MasterCatalogApiPath.BASE_PATH + MasterCatalogApiPath.FETCH_INDICATIVE_PRICE).contentType(
        MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON)
      .content(new ObjectMapper().writeValueAsString(request));
    mockMvc.perform(requestBuilder).andExpect(status().isOk())
      .andExpect(jsonPath("$.success", equalTo(true)));
    Mockito.verify(masterSkuReviewService).fetchIndicativePrice(request);
    verify(clientParameterHelper).getRequestId();
  }

  private void mockFile(String filePath) throws IOException {
    File file = new File(filePath);
    FileUtils.writeByteArrayToFile(file, fileContent);
  }

}
