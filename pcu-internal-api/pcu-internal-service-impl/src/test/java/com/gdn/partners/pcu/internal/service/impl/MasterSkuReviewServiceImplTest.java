package com.gdn.partners.pcu.internal.service.impl;

import static org.junit.jupiter.api.Assertions.assertEquals;

import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.common.web.wrapper.response.PageMetaData;
import com.gdn.mta.bulk.dto.product.constant.DomainEventName;
import com.gdn.partners.pcu.internal.client.feign.MasterCatalogFeign;
import com.gdn.partners.pcu.internal.client.model.request.ChangeAssigneeRequest;
import com.gdn.partners.pcu.internal.client.model.request.ClusterReviewFeedbackRequest;
import com.gdn.partners.pcu.internal.client.model.request.AcceptRejectActionRequest;
import com.gdn.partners.pcu.internal.client.model.request.InReviewListWebRequest;
import com.gdn.partners.pcu.internal.client.model.request.ItemSkuListRequest;
import com.gdn.partners.pcu.internal.client.model.request.MasterSkuItemsListWebRequest;
import com.gdn.partners.pcu.internal.client.model.response.ClusterActionResponse;
import com.gdn.partners.pcu.internal.client.model.response.CompareAnchorResponse;
import com.gdn.partners.pcu.internal.client.model.response.InReviewListResponse;
import com.gdn.partners.pcu.internal.client.model.response.ItemHistoryResponse;
import com.gdn.partners.pcu.internal.client.model.response.ItemSkuAndIndicativePriceResponse;
import com.gdn.partners.pcu.internal.client.model.response.ItemSkuDetailResponse;
import com.gdn.partners.pcu.internal.client.model.response.ItemsListingResponse;
import com.gdn.partners.pcu.internal.client.model.response.MasterSkuConfigResponse;
import com.gdn.partners.pcu.internal.model.Constants;
import com.gdn.partners.pcu.internal.service.FileStorageService;
import com.gdn.partners.pcu.internal.service.PartnersEngineService;
import com.gdn.partners.pcu.internal.service.impl.config.KafkaPublisher;
import com.gdn.partners.pcu.internal.service.impl.event.model.BulkReviewUploadModel;
import com.gdn.partners.pcu.internal.service.impl.exception.ClientException;
import com.gdn.partners.pcu.internal.service.impl.exception.ConstraintViolationException;
import com.gdn.partners.pcu.internal.service.impl.exception.InvalidStateException;
import com.gdn.partners.pcu.internal.service.model.BulkInternalProcessType;
import com.gdn.partners.pcu.internal.streaming.model.bulk.BulkProcessEntity;
import com.gdn.partners.pcu.internal.streaming.model.bulk.MasterSkuReviewDownloadItemsRequest;
import com.gdn.partners.pcu.internal.web.model.request.MasterSkuItemsDownloadWebRequest;

import com.gdn.partners.pcu.internal.web.model.request.AnchorMappingWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.MasterSkuInReviewDownloadWebRequest;
import org.apache.commons.io.FileUtils;
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

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import org.mockito.MockitoAnnotations;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;
import org.springframework.mock.web.MockMultipartFile;
import org.springframework.test.util.ReflectionTestUtils;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
class MasterSkuReviewServiceImplTest {
  private static final String ITEM_SKU = "item-sku";
  private static final String MASTER_SKU = "master-sku";
  private static final String REQUEST_ID = "request-id";
  private static final String ERROR_MSG = "error";
  private static final String KEYWORD = "keyword";
  private static final String CATEGORY_CODE = "categoryCode";
  private static final String FIRST_ANCHOR = "firstAnchor";
  private static final String SECOND_ANCHOR = "secondAnchor";
  private static final String USERNAME = "username";
  private static final String ASSIGNEE = "assignee";
  private static final int PAGE = 0;
  private static final int SIZE = 10;
  private byte[] fileContent;
  private static final String PATH = "path";
  private static final String FILE = "file";
  private static final String TYPE = "type";
  private static final String ORIGINAL_FILENAME = "originalFilename.xls";
  private static final String STORE_ID = "store-id";

  @InjectMocks
  private MasterSkuReviewServiceImpl masterSkuReviewService;

  @Mock
  private MasterCatalogFeign masterCatalogFeign;

  @Mock
  private PartnersEngineService partnersEngineService;

  @Mock
  private KafkaPublisher kafkaProducer;

  @Mock
  private FileStorageService fileStorageService;

  @Captor
  private ArgumentCaptor<MasterSkuReviewDownloadItemsRequest> masterSkuAllItemsDownloadRequestArgumentCaptor;

  @Captor
  private ArgumentCaptor<BulkReviewUploadModel> bulkReviewUploadModelArgumentCaptor;

  private MasterSkuItemsDownloadWebRequest masterSkuItemsDownloadWebRequest;

  @BeforeEach
  public void init() {
    MockitoAnnotations.openMocks(this);
    masterSkuItemsDownloadWebRequest = new MasterSkuItemsDownloadWebRequest();
    fileContent = new byte[] {-1, -40, -20, -10};
  }

  @AfterEach
  public void teardown() {
    Mockito.verifyNoMoreInteractions(masterCatalogFeign);
  }

  @Test
  void getInReviewProductListTest() {
    InReviewListWebRequest request = new InReviewListWebRequest();
    GdnRestListResponse<InReviewListResponse> serviceResponse =
      new GdnRestListResponse<>(new ArrayList<>(), new PageMetaData(10, 0, 10),
        Constants.REQUEST_ID);
    Mockito.when(masterCatalogFeign.getItemsForInReview(PAGE, SIZE, request))
        .thenReturn(serviceResponse);
    GdnRestListResponse<InReviewListResponse> result =
        masterSkuReviewService.getInReviewProductList(PAGE, SIZE, request);
    verify(masterCatalogFeign, times(1)).getItemsForInReview(PAGE, SIZE, request);
    assertEquals(serviceResponse, result);
  }

  @Test
  void getAllMasterSkuItemsListTest() {
    ReflectionTestUtils.setField(masterSkuReviewService, "pdpLinkPrefix", ITEM_SKU);
    ItemsListingResponse itemsListingResponse = new ItemsListingResponse();
    itemsListingResponse.setItemSku(ITEM_SKU);
    MasterSkuItemsListWebRequest request = new MasterSkuItemsListWebRequest();
    GdnRestListResponse<ItemsListingResponse> serviceResponse =
        new GdnRestListResponse<>(new ArrayList<>(List.of(itemsListingResponse)), new PageMetaData(10, 0, 10),
            Constants.REQUEST_ID);
    Mockito.when(masterCatalogFeign.getAllMasterSkuItemsList(PAGE, SIZE, request)).thenReturn(serviceResponse);

    GdnRestListResponse<ItemsListingResponse> result =
        masterSkuReviewService.getAllMasterSkuItemsList(PAGE, SIZE, request);

    verify(masterCatalogFeign, times(1)).getAllMasterSkuItemsList(PAGE, SIZE, request);
    assertEquals(serviceResponse, result);
  }

  @Test
  void getMasterSkuDetailsTest() {
    ReflectionTestUtils.setField(masterSkuReviewService, "pdpLinkPrefix", ITEM_SKU);
    ItemSkuDetailResponse itemSkuDetailResponse = new ItemSkuDetailResponse();
    GdnRestSingleResponse<ItemSkuDetailResponse> response =
        new GdnRestSingleResponse<>(itemSkuDetailResponse, Constants.REQUEST_ID);
    Mockito.when(masterCatalogFeign.getMasterSkuDetails(MASTER_SKU, true)).thenReturn(response);
    GdnRestSingleResponse<ItemSkuDetailResponse> gdnRestSingleResponse =
        masterSkuReviewService.getMasterSkuDetails(MASTER_SKU, true);
    verify(masterCatalogFeign).getMasterSkuDetails(MASTER_SKU, true);
    assertEquals(response, gdnRestSingleResponse);
  }

  @Test
  void getMasterSkuDetailsSuccessFalseTest() {
    ReflectionTestUtils.setField(masterSkuReviewService, "pdpLinkPrefix", ITEM_SKU);
    ItemSkuDetailResponse itemSkuDetailResponse = new ItemSkuDetailResponse();

    GdnRestSingleResponse<ItemSkuDetailResponse> response =
        new GdnRestSingleResponse<>(null, null, false, itemSkuDetailResponse, REQUEST_ID);
    Mockito.when(masterCatalogFeign.getMasterSkuDetails(MASTER_SKU, true)).thenReturn(response);
    try {
      GdnRestSingleResponse<ItemSkuDetailResponse> gdnRestSingleResponse =
          masterSkuReviewService.getMasterSkuDetails(MASTER_SKU, true);
    } catch (ClientException ex) {
      Assertions.assertNotNull(ex);
    } finally {
      verify(masterCatalogFeign).getMasterSkuDetails(MASTER_SKU, true);
    }
  }

  @Test
  void getMasterSkuDetailsNullResponseTest() {
    ReflectionTestUtils.setField(masterSkuReviewService, "pdpLinkPrefix", ITEM_SKU);
    ItemSkuDetailResponse itemSkuDetailResponse = new ItemSkuDetailResponse();
    Mockito.when(masterCatalogFeign.getMasterSkuDetails(MASTER_SKU, true)).thenReturn(null);
    try {
      GdnRestSingleResponse<ItemSkuDetailResponse> gdnRestSingleResponse =
          masterSkuReviewService.getMasterSkuDetails(MASTER_SKU, true);
    } catch (ClientException ex){
      Assertions.assertNotNull(ex);
    } finally {
      verify(masterCatalogFeign).getMasterSkuDetails(MASTER_SKU, true);
    }
  }

  @Test
  void getMasterSkuDetailsSuccessFalseValidationTest() {
    ReflectionTestUtils.setField(masterSkuReviewService, "pdpLinkPrefix", ITEM_SKU);
    ItemSkuDetailResponse itemSkuDetailResponse = new ItemSkuDetailResponse();

    GdnRestSingleResponse<ItemSkuDetailResponse> response =
        new GdnRestSingleResponse<>(null, ErrorCategory.VALIDATION.getCode(), false, itemSkuDetailResponse, REQUEST_ID);
    Mockito.when(masterCatalogFeign.getMasterSkuDetails(MASTER_SKU, true)).thenReturn(response);
    try {
      GdnRestSingleResponse<ItemSkuDetailResponse> gdnRestSingleResponse =
          masterSkuReviewService.getMasterSkuDetails(MASTER_SKU, true);
    } catch (ConstraintViolationException ex){
      Assertions.assertNotNull(ex);
    } finally {
      verify(masterCatalogFeign).getMasterSkuDetails(MASTER_SKU, true);
    }
  }


  @Test
  void testGetCompareAnchorDetails_Success() {
    String firstAnchor = "anchor1";
    String secondAnchor = "anchor2";
    CompareAnchorResponse mockResponse = new CompareAnchorResponse();
    GdnRestSingleResponse<CompareAnchorResponse> mockSingleBaseResponse =
      new GdnRestSingleResponse(null, null, true, mockResponse, REQUEST_ID);
    Mockito.when(masterCatalogFeign.getCompareAnchorSkuDetails(firstAnchor, secondAnchor))
      .thenReturn(mockSingleBaseResponse);
    GdnRestSingleResponse<CompareAnchorResponse> response =
      masterSkuReviewService.getCompareAnchorDetails(firstAnchor, secondAnchor);
    assertNotNull(response);
    verify(masterCatalogFeign, times(1)).getCompareAnchorSkuDetails(firstAnchor, secondAnchor);
  }

  @Test
  void fetchItemsMappedToMasterSkuTest() {
    ReflectionTestUtils.setField(masterSkuReviewService, "pdpLinkPrefix", ITEM_SKU);
    ItemSkuDetailResponse itemSkuDetailResponse = new ItemSkuDetailResponse();
    itemSkuDetailResponse.setItemSku(ITEM_SKU);
    GdnRestListResponse<ItemSkuDetailResponse> serviceResponse =
        new GdnRestListResponse<>(new ArrayList<>(List.of(itemSkuDetailResponse)), new PageMetaData(10, 0, 10),
            Constants.REQUEST_ID);
    Mockito.when(masterCatalogFeign.fetchItemsMappedToMasterSku(PAGE, SIZE, MASTER_SKU)).thenReturn(serviceResponse);

    GdnRestListResponse<ItemSkuDetailResponse> result =
        masterSkuReviewService.fetchItemsMappedToMasterSku(PAGE, SIZE, MASTER_SKU);

    verify(masterCatalogFeign, times(1)).fetchItemsMappedToMasterSku(PAGE, SIZE, MASTER_SKU);
    assertEquals(serviceResponse, result);
  }

  @Test
  void fetchItemHistoryDetailsTest() {
    ItemHistoryResponse itemHistoryResponse = new ItemHistoryResponse();
    itemHistoryResponse.setItemSku(ITEM_SKU);
    GdnRestListResponse<ItemHistoryResponse> serviceResponse =
        new GdnRestListResponse<>(new ArrayList<>(List.of(itemHistoryResponse)), new PageMetaData(10, 0, 10),
            Constants.REQUEST_ID);
    Mockito.when(masterCatalogFeign.fetchItemHistoryDetails(PAGE, SIZE, ITEM_SKU)).thenReturn(serviceResponse);

    GdnRestListResponse<ItemHistoryResponse> result =
        masterSkuReviewService.fetchItemHistoryDetails(PAGE, SIZE, ITEM_SKU);

    verify(masterCatalogFeign, times(1)).fetchItemHistoryDetails(PAGE, SIZE, ITEM_SKU);
    assertEquals(serviceResponse, result);
  }

  @Test
  void fetchItemHistoryDetailsSuccessFalseTest() {
    ItemHistoryResponse itemHistoryResponse = new ItemHistoryResponse();
    itemHistoryResponse.setItemSku(ITEM_SKU);
    GdnRestListResponse<ItemHistoryResponse> serviceResponse = new GdnRestListResponse<>(null, null, false, REQUEST_ID);
    Mockito.when(masterCatalogFeign.fetchItemHistoryDetails(PAGE, SIZE, ITEM_SKU)).thenReturn(serviceResponse);
    try {
      GdnRestListResponse<ItemHistoryResponse> result =
          masterSkuReviewService.fetchItemHistoryDetails(PAGE, SIZE, ITEM_SKU);
    } catch (ClientException ex) {
      Assertions.assertNotNull(ex);
    } finally {
      verify(masterCatalogFeign, times(1)).fetchItemHistoryDetails(PAGE, SIZE, ITEM_SKU);
    }
  }

  @Test
  void fetchItemHistoryDetailsSuccessFalseValidationTest() {
    ItemHistoryResponse itemHistoryResponse = new ItemHistoryResponse();
    itemHistoryResponse.setItemSku(ITEM_SKU);
    GdnRestListResponse<ItemHistoryResponse> serviceResponse =
        new GdnRestListResponse<>(null, ErrorCategory.VALIDATION.getCode(), false, REQUEST_ID);
    Mockito.when(masterCatalogFeign.fetchItemHistoryDetails(PAGE, SIZE, ITEM_SKU)).thenReturn(serviceResponse);
    try {
      GdnRestListResponse<ItemHistoryResponse> result =
          masterSkuReviewService.fetchItemHistoryDetails(PAGE, SIZE, ITEM_SKU);
    } catch (ConstraintViolationException ex){
      Assertions.assertNotNull(ex);
    } finally {
      verify(masterCatalogFeign, times(1)).fetchItemHistoryDetails(PAGE, SIZE, ITEM_SKU);
    }
  }

  @Test
  void fetchItemHistoryDetailsNullResponseTest() {
    ItemHistoryResponse itemHistoryResponse = new ItemHistoryResponse();
    itemHistoryResponse.setItemSku(ITEM_SKU);
    Mockito.when(masterCatalogFeign.fetchItemHistoryDetails(PAGE, SIZE, ITEM_SKU)).thenReturn(null);
    try {
      GdnRestListResponse<ItemHistoryResponse> result =
          masterSkuReviewService.fetchItemHistoryDetails(PAGE, SIZE, ITEM_SKU);
    } catch (ClientException ex){
      Assertions.assertNotNull(ex);
    } finally {
      verify(masterCatalogFeign, times(1)).fetchItemHistoryDetails(PAGE, SIZE, ITEM_SKU);
    }
  }

  @Test
  void fetchMasterSkuReviewConfigTest() {
    MasterSkuConfigResponse masterSkuConfigResponse = new MasterSkuConfigResponse();
    GdnRestSingleResponse<MasterSkuConfigResponse> response =
        new GdnRestSingleResponse<>(masterSkuConfigResponse, Constants.REQUEST_ID);
    GdnRestSingleResponse<ItemSkuDetailResponse> mockSingleBaseResponse =
        new GdnRestSingleResponse(null, null, true, masterSkuConfigResponse, REQUEST_ID);
    Mockito.when(masterCatalogFeign.fetchMasterSkuReviewConfig())
        .thenReturn(response);
    GdnBaseRestResponse result = masterSkuReviewService.fetchMasterSkuReviewConfig();
    verify(masterCatalogFeign, times(1)).fetchMasterSkuReviewConfig();
    assertEquals(response, result);
  }

  @Test
  void performClusterReviewActionTest() {
    ClusterReviewFeedbackRequest clusterReviewFeedbackRequest = new ClusterReviewFeedbackRequest();
    GdnRestSingleResponse<ClusterActionResponse> response =
      new GdnRestSingleResponse<ClusterActionResponse>();
    response.setSuccess(true);
    Mockito.when(masterCatalogFeign.performClusterReviewAction(MASTER_SKU, clusterReviewFeedbackRequest))
        .thenReturn(response);
    GdnBaseRestResponse result = masterSkuReviewService.performClusterAction(MASTER_SKU, clusterReviewFeedbackRequest);
    verify(masterCatalogFeign, times(1)).performClusterReviewAction(MASTER_SKU, clusterReviewFeedbackRequest);
    assertEquals(response, result);
  }

  @Test
  void performClusterReviewActionNullResponseTest() {
    ClusterReviewFeedbackRequest clusterReviewFeedbackRequest = new ClusterReviewFeedbackRequest();
    Mockito.when(masterCatalogFeign.performClusterReviewAction(MASTER_SKU, clusterReviewFeedbackRequest))
        .thenReturn(null);
    try {
      GdnBaseRestResponse result =
          masterSkuReviewService.performClusterAction(MASTER_SKU, clusterReviewFeedbackRequest);
    } catch (ClientException ex){
      Assertions.assertNotNull(ex);
    } finally {
      verify(masterCatalogFeign, times(1)).performClusterReviewAction(MASTER_SKU, clusterReviewFeedbackRequest);
    }
  }

  @Test
  void performClusterReviewActionSuccessFalseTest() {
    ClusterReviewFeedbackRequest clusterReviewFeedbackRequest = new ClusterReviewFeedbackRequest();
    GdnRestSingleResponse<ClusterActionResponse> response =  new GdnRestSingleResponse<>(ERROR_MSG,
      null, false, null, null);
    Mockito.when(masterCatalogFeign.performClusterReviewAction(MASTER_SKU, clusterReviewFeedbackRequest))
        .thenReturn(response);
    try {
      GdnBaseRestResponse result =
          masterSkuReviewService.performClusterAction(MASTER_SKU, clusterReviewFeedbackRequest);
    } catch (ClientException ex){
      Assertions.assertNotNull(ex);
    } finally {
      verify(masterCatalogFeign, times(1)).performClusterReviewAction(MASTER_SKU, clusterReviewFeedbackRequest);
    }
  }

  @Test
  void performClusterReviewActionSuccessFalseValidationTest() {
    ClusterReviewFeedbackRequest clusterReviewFeedbackRequest = new ClusterReviewFeedbackRequest();
    GdnRestSingleResponse<ClusterActionResponse> response =
      new GdnRestSingleResponse<>(ERROR_MSG, ErrorCategory.VALIDATION.getCode(), false, null, null);
    Mockito.when(
        masterCatalogFeign.performClusterReviewAction(MASTER_SKU, clusterReviewFeedbackRequest))
      .thenReturn(response);
    try {
      GdnBaseRestResponse result =
          masterSkuReviewService.performClusterAction(MASTER_SKU, clusterReviewFeedbackRequest);
    } catch (ConstraintViolationException ex){
      Assertions.assertNotNull(ex);
    } finally {
      verify(masterCatalogFeign, times(1)).performClusterReviewAction(MASTER_SKU, clusterReviewFeedbackRequest);
    }
  }

  @Test
  void updateAcceptRejectActionRequestTest() {
    String firstAnchor = "firstAnchor";
    String secondAnchor = "secondAnchor";
    AcceptRejectActionRequest request = new AcceptRejectActionRequest();

    GdnBaseRestResponse serviceResponse =
        new GdnBaseRestResponse(null, null, true, Constants.REQUEST_ID);
    Mockito.when(
        masterCatalogFeign.updateAnchorsForAcceptRejectAction(firstAnchor, secondAnchor, request))
        .thenReturn(serviceResponse);
    masterSkuReviewService.updateAcceptRejectActionRequest(firstAnchor, secondAnchor, request);

    verify(masterCatalogFeign, times(1)).updateAnchorsForAcceptRejectAction(firstAnchor,
        secondAnchor, request);
  }

  @Test
  void updateChangeAssigneeActionTest() throws Exception {
    ChangeAssigneeRequest request = new ChangeAssigneeRequest();
    request.setNewAssignee("VENDORS");
    GdnBaseRestResponse serviceResponse =
      new GdnBaseRestResponse(null, null, true, Constants.REQUEST_ID);
    Mockito.when(masterCatalogFeign.updateAssignedTo(request)).thenReturn(serviceResponse);
    Mockito.when(partnersEngineService.getMasterSkuReviewers()).thenReturn(List.of("VENDORS"));
    masterSkuReviewService.updateChangeAssigneeAction(request);
    Mockito.verify(partnersEngineService).getMasterSkuReviewers();
    verify(masterCatalogFeign, times(1)).updateAssignedTo(request);
  }

  @Test
  void getMasterSkuReviewReviewersTest() throws Exception {
    Mockito.when(partnersEngineService.getMasterSkuReviewers())
      .thenReturn(Arrays.asList("VENDORS"));
    List<String> result = masterSkuReviewService.getMasterSkuReviewReviewers();
    Mockito.verify(partnersEngineService).getMasterSkuReviewers();
    Assertions.assertEquals(1, result.size());
  }

  @Test
  void downloadAllItemsForMasterSkuReviewTest() {
    masterSkuItemsDownloadWebRequest.setKeyword(KEYWORD);
    masterSkuItemsDownloadWebRequest.setCategoryCode(CATEGORY_CODE);
    this.masterSkuReviewService.downloadItemsForMasterSkuReview(Constants.USER_NAME, masterSkuItemsDownloadWebRequest,
        PAGE, SIZE);
    Mockito.verify(this.kafkaProducer)
        .send(Mockito.eq(DomainEventName.BULK_DOWNLOAD_ALL_EVENT), Mockito.eq(Constants.USER_NAME),
            masterSkuAllItemsDownloadRequestArgumentCaptor.capture());
    MasterSkuReviewDownloadItemsRequest masterSkuReviewDownloadItemsRequest =
        masterSkuAllItemsDownloadRequestArgumentCaptor.getValue();
    Assertions.assertNotNull(masterSkuReviewDownloadItemsRequest);
    Assertions.assertEquals(KEYWORD, masterSkuReviewDownloadItemsRequest.getKeyword());
    Assertions.assertEquals(CATEGORY_CODE, masterSkuReviewDownloadItemsRequest.getCategoryCode());
    Assertions.assertEquals(BulkProcessEntity.MASTER_SKU_ALL_ITEMS_DOWNLOAD,
        masterSkuReviewDownloadItemsRequest.getBulkProcessEntity());
  }

  @Test
  void downloadSelectedItemsForMasterSkuReviewTest() {
    masterSkuItemsDownloadWebRequest.setItemSkuList(List.of(ITEM_SKU));
    this.masterSkuReviewService.downloadItemsForMasterSkuReview(Constants.USER_NAME, masterSkuItemsDownloadWebRequest,
        PAGE, SIZE);
    Mockito.verify(this.kafkaProducer)
        .send(Mockito.eq(DomainEventName.BULK_DOWNLOAD_ALL_EVENT), Mockito.eq(Constants.USER_NAME),
            masterSkuAllItemsDownloadRequestArgumentCaptor.capture());
    MasterSkuReviewDownloadItemsRequest masterSkuSelectedItemsDownloadRequest =
        masterSkuAllItemsDownloadRequestArgumentCaptor.getValue();
    Assertions.assertNotNull(masterSkuSelectedItemsDownloadRequest);
    Assertions.assertEquals(masterSkuItemsDownloadWebRequest.getItemSkuList(),
        masterSkuSelectedItemsDownloadRequest.getItemSkuList());
    Assertions.assertEquals(BulkProcessEntity.MASTER_SKU_SELECTED_ITEMS_DOWNLOAD,
        masterSkuSelectedItemsDownloadRequest.getBulkProcessEntity());
  }

  @Test
  void testBulkDownloadInReviewAnchorMappings() {
    AnchorMappingWebRequest anchorMappingWebRequest = new AnchorMappingWebRequest();
    anchorMappingWebRequest.setFirstAnchorSku(FIRST_ANCHOR);
    anchorMappingWebRequest.setSecondAnchorSku(SECOND_ANCHOR);
    List<AnchorMappingWebRequest> anchorMappingWebRequestList = new ArrayList<>();
    anchorMappingWebRequestList.add(anchorMappingWebRequest);
    MasterSkuInReviewDownloadWebRequest request = new MasterSkuInReviewDownloadWebRequest();
    request.setAssignedTo(ASSIGNEE);
    request.setClusterRequestList(anchorMappingWebRequestList);
    masterSkuReviewService.bulkDownloadInReviewAnchorMappings(USERNAME, request, PAGE, SIZE);
    Mockito.verify(kafkaProducer).send(Mockito.eq(DomainEventName.BULK_DOWNLOAD_ALL_EVENT), Mockito.eq(USERNAME),
      Mockito.any());
  }

  @Test
  void testBulkDownloadInReviewEmptyAnchorMappings() {
    MasterSkuInReviewDownloadWebRequest request = new MasterSkuInReviewDownloadWebRequest();
    request.setAssignedTo(ASSIGNEE);
    request.setClusterRequestList(new ArrayList<>());
    masterSkuReviewService.bulkDownloadInReviewAnchorMappings(USERNAME, request, PAGE, SIZE);
    Mockito.verify(kafkaProducer)
        .send(Mockito.eq(DomainEventName.BULK_DOWNLOAD_ALL_EVENT), Mockito.eq(USERNAME),
            Mockito.any());
  }

  @Test
  void uploadBulkFileTest() throws Exception {
    mockFile();
    MockMultipartFile multipartFile =
      new MockMultipartFile("request", ORIGINAL_FILENAME, "application/vnd.ms-excel",
        FileUtils.readFileToByteArray(new File(PATH + FILE)));
    when(fileStorageService.uploadFilePath(any(), anyString(), anyString())).thenReturn(PATH);
    masterSkuReviewService.uploadBulkFile(multipartFile,
      BulkInternalProcessType.MASTER_SKU_BULK_REVIEW.name(), REQUEST_ID, STORE_ID, USERNAME);
    verify(fileStorageService).uploadFilePath(any(), anyString(), anyString());
    verify(kafkaProducer).send(eq(DomainEventName.BULK_REVIEW_UPLOAD_EVENT), anyString(),
      bulkReviewUploadModelArgumentCaptor.capture());
    assertNotNull(bulkReviewUploadModelArgumentCaptor.getValue());
    assertEquals(BulkInternalProcessType.MASTER_SKU_BULK_REVIEW.name(),
      bulkReviewUploadModelArgumentCaptor.getValue().getBulkProcessType());
    assertEquals(REQUEST_ID, bulkReviewUploadModelArgumentCaptor.getValue().getRequestId());
    assertEquals(STORE_ID, bulkReviewUploadModelArgumentCaptor.getValue().getStoreId());
    assertEquals(USERNAME, bulkReviewUploadModelArgumentCaptor.getValue().getCreatedBy());
  }

  @Test
  void updateChangeAssigneeActionInvalidUserTest() throws Exception {
    ChangeAssigneeRequest request = new ChangeAssigneeRequest();
    request.setNewAssignee("RANDOM");
    GdnBaseRestResponse serviceResponse =
      new GdnBaseRestResponse(null, null, true, Constants.REQUEST_ID);
    Mockito.when(masterCatalogFeign.updateAssignedTo(request)).thenReturn(serviceResponse);
    Mockito.when(partnersEngineService.getMasterSkuReviewers()).thenReturn(List.of("VENDORS"));
    try {
      masterSkuReviewService.updateChangeAssigneeAction(request);
    }
    catch (InvalidStateException ex){
      Assertions.assertNotNull(ex);
      Mockito.verify(partnersEngineService).getMasterSkuReviewers();
    }
  }

  @Test
  void unAssigneeActionTest() throws Exception {
    ChangeAssigneeRequest request = new ChangeAssigneeRequest();
    request.setNewAssignee("");
    GdnBaseRestResponse serviceResponse =
      new GdnBaseRestResponse(null, null, true, Constants.REQUEST_ID);
    Mockito.when(masterCatalogFeign.updateAssignedTo(request)).thenReturn(serviceResponse);
    masterSkuReviewService.updateChangeAssigneeAction(request);
    verify(masterCatalogFeign, times(1)).updateAssignedTo(request);
  }

  @Test
  void fetchIndicativePriceTest() {
    ItemSkuListRequest request = new ItemSkuListRequest();
    request.setItemSkuList(List.of(ITEM_SKU));
    ItemSkuAndIndicativePriceResponse indicativePriceResponse =
      new ItemSkuAndIndicativePriceResponse();
    Mockito.when(masterCatalogFeign.fetchIndicativePrice(request))
      .thenReturn(new GdnRestSingleResponse<>(indicativePriceResponse, USERNAME));
    masterSkuReviewService.fetchIndicativePrice(request);
    Mockito.verify(masterCatalogFeign).fetchIndicativePrice(request);
  }

  @Test
  void fetchIndicativePriceEmptyListTest() {
    ItemSkuListRequest request = new ItemSkuListRequest();
    ItemSkuAndIndicativePriceResponse response =
      masterSkuReviewService.fetchIndicativePrice(request);
    assertEquals(0, response.getItemSkuToIndicativePriceDetailsMap().size());
  }

  @Test
  void fetchIndicativePriceNullTest() {
    ItemSkuAndIndicativePriceResponse response = masterSkuReviewService.fetchIndicativePrice(null);
    assertEquals(0, response.getItemSkuToIndicativePriceDetailsMap().size());
  }

  private void mockFile() throws IOException {
    File file = new File("pathfile");
    FileUtils.writeByteArrayToFile(file, fileContent);
  }
}
