package com.gdn.partners.pcu.internal.client.fallback;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;

import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.partners.pcu.internal.client.model.request.BigQueryFetchRequest;
import com.gdn.partners.pcu.internal.client.model.request.ChangeAssigneeRequest;
import com.gdn.partners.pcu.internal.client.model.request.ClusterReviewFeedbackRequest;
import com.gdn.partners.pcu.internal.client.model.request.AcceptRejectActionRequest;
import com.gdn.partners.pcu.internal.client.model.request.ItemSkuListRequest;
import com.gdn.partners.pcu.internal.client.model.request.MasterSkuItemsListWebRequest;
import com.gdn.partners.pcu.internal.client.model.response.CompareAnchorResponse;
import com.gdn.partners.pcu.internal.client.model.response.ItemHistoryResponse;
import com.gdn.partners.pcu.internal.client.model.response.ItemSkuAndIndicativePriceResponse;
import com.gdn.partners.pcu.internal.client.model.response.ItemSkuDetailResponse;
import com.gdn.partners.pcu.internal.client.model.response.ItemsListingResponse;
import com.gdn.partners.pcu.internal.client.model.response.MasterSkuConfigResponse;
import com.gdn.partners.pcu.internal.model.ErrorMessages;
import com.gdn.partners.pcu.internal.client.model.request.InReviewListWebRequest;
import com.gdn.partners.pcu.internal.client.model.response.InReviewListResponse;
import com.gdn.x.campaign.clientsdk.shade.com.gdn.common.enums.ErrorCategory;
import org.junit.jupiter.api.Test;

public class MasterCatalogFeignFallbackTest {

  private static final String MASTER_SKU = "master-sku";
  private static final String PROCESS_NAME = "processName";
  private static final int PAGE = 0;
  private static final int SIZE = 10;


  private final MasterCatalogFeignFallback feignFallback = new MasterCatalogFeignFallback();

  @Test
  public void getItemsForInReviewTest() {
    InReviewListWebRequest request = new InReviewListWebRequest();
    GdnRestListResponse<InReviewListResponse> response =
        feignFallback.getItemsForInReview(PAGE, SIZE, request);

    assertNotNull(response);
    assertFalse(response.isSuccess());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertNull(response.getContent());
    assertNull(response.getRequestId());
  }

  @Test
  public void getAllMasterSkuItemsList() {
    MasterSkuItemsListWebRequest request = new MasterSkuItemsListWebRequest();
    GdnRestListResponse<ItemsListingResponse> response = feignFallback.getAllMasterSkuItemsList(PAGE, SIZE, request);
    assertNotNull(response);
    assertFalse(response.isSuccess());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());

  }

  @Test
  public void fetchItemsMappedToMasterSkuTest() {
    GdnRestListResponse<ItemSkuDetailResponse> response =
        feignFallback.fetchItemsMappedToMasterSku(PAGE, SIZE, MASTER_SKU);
    assertNotNull(response);
    assertFalse(response.isSuccess());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
  }

  @Test
  public void getMasterSkuDetailsTest() {
    GdnRestSingleResponse<ItemSkuDetailResponse> response =
      feignFallback.getMasterSkuDetails(MASTER_SKU, true);
    assertEquals(com.gdn.common.enums.ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }

  @Test
  public void testGetCompareAnchorSkuDetails_FallbackError() {
    GdnRestSingleResponse<CompareAnchorResponse> response =
      feignFallback.getCompareAnchorSkuDetails("anchor1", "anchor2");
    assertNotNull(response);
    assertFalse(response.isSuccess());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertNull(response.getValue());
  }

  @Test
  public void fetchItemHistoryDetailsTest() {
    GdnRestListResponse<ItemHistoryResponse> response = feignFallback.fetchItemHistoryDetails(PAGE, SIZE, MASTER_SKU);
    assertNotNull(response);
    assertFalse(response.isSuccess());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
  }

  @Test
  public void fetchMasterSkuReviewConfigTest() {
    GdnRestSingleResponse<MasterSkuConfigResponse> response = feignFallback.fetchMasterSkuReviewConfig();
    assertNotNull(response);
    assertFalse(response.isSuccess());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
  }

  @Test
  public void performClusterReviewActionTest() {
    ClusterReviewFeedbackRequest clusterReviewFeedbackRequest = new ClusterReviewFeedbackRequest();
    GdnBaseRestResponse response = feignFallback.performClusterReviewAction(MASTER_SKU, clusterReviewFeedbackRequest);
    assertNotNull(response);
    assertFalse(response.isSuccess());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
  }

  @Test
  public void updateAnchorsForAcceptRejectActionTest() {
    String firstAnchor = "firstAnchor";
    String secondAnchor = "secondAnchor";
    AcceptRejectActionRequest request = new AcceptRejectActionRequest();
    GdnBaseRestResponse response =
        feignFallback.updateAnchorsForAcceptRejectAction(firstAnchor, secondAnchor, request);
    assertNotNull(response);
    assertFalse(response.isSuccess());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
  }

  @Test
  public void updateAssignedToTest() {
    ChangeAssigneeRequest request = new ChangeAssigneeRequest();
    GdnBaseRestResponse response = feignFallback.updateAssignedTo(request);
    assertNotNull(response);
    assertFalse(response.isSuccess());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
  }

  @Test
  public void fetchDataFromBigQueryTest() {
    BigQueryFetchRequest request = new BigQueryFetchRequest();
    GdnBaseRestResponse response = feignFallback.fetchDataFromBigQuery(PROCESS_NAME, request);
    assertNotNull(response);
    assertFalse(response.isSuccess());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
  }

  @Test
  public void testFetchIndicativePrice() {
    ItemSkuListRequest request = new ItemSkuListRequest();
    GdnRestSingleResponse<ItemSkuAndIndicativePriceResponse> response =
      feignFallback.fetchIndicativePrice(request);
    assertNotNull(response);
    assertFalse(response.isSuccess());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertNull(response.getValue());
  }
}
