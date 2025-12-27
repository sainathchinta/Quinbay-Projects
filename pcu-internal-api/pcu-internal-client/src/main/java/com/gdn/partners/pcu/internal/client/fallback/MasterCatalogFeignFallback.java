package com.gdn.partners.pcu.internal.client.fallback;

import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.partners.pcu.internal.client.feign.MasterCatalogFeign;
import com.gdn.partners.pcu.internal.client.model.request.BigQueryFetchRequest;
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
import com.gdn.partners.pcu.internal.model.ErrorMessages;
import com.gdn.partners.pcu.internal.client.model.request.InReviewListWebRequest;
import com.gdn.partners.pcu.internal.client.model.response.InReviewListResponse;
import org.springframework.stereotype.Component;

/**
 * @author Navya Naveli
 */

@Component
public class MasterCatalogFeignFallback implements MasterCatalogFeign {

  @Override
  public GdnRestListResponse<InReviewListResponse> getItemsForInReview(int page, int size,
      InReviewListWebRequest inReviewListWebRequest) {
    return new GdnRestListResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE,
        ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, null, null);
  }

  @Override
  public GdnRestListResponse<ItemsListingResponse> getAllMasterSkuItemsList(int page, int size,
      MasterSkuItemsListWebRequest request) {
    return new GdnRestListResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE,
        ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, null, null);
  }

  @Override
  public GdnRestListResponse<ItemSkuDetailResponse> fetchItemsMappedToMasterSku(int page, int size, String masterItemSku) {
    return new GdnRestListResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE,
        ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, null, null);
  }

  @Override
  public GdnRestSingleResponse<ItemSkuDetailResponse> getMasterSkuDetails(String masterItemSku,
    boolean allData) {
    return new GdnRestSingleResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE,
        ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, null);
  }

  @Override
  public GdnRestSingleResponse<CompareAnchorResponse> getCompareAnchorSkuDetails(String firstAnchor,
    String secondAnchor) {
    return new GdnRestSingleResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE,
      ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, null);
  }

  @Override
  public GdnRestListResponse<ItemHistoryResponse> fetchItemHistoryDetails(int page, int size, String itemSku) {
    return new GdnRestListResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE,
        ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, null, null);
  }

  @Override
  public GdnRestSingleResponse<MasterSkuConfigResponse> fetchMasterSkuReviewConfig() {
    return new GdnRestSingleResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE,
        ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, null);
  }

  @Override
  public GdnRestSingleResponse<ClusterActionResponse> performClusterReviewAction(
    String masterItemSku, ClusterReviewFeedbackRequest request) {
    return new GdnRestSingleResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE,
      ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, null);
  }

  @Override
  public GdnBaseRestResponse updateAnchorsForAcceptRejectAction(String firstAnchorSku,
      String secondAnchorSku, AcceptRejectActionRequest request) {
    return new GdnBaseRestResponse(ErrorMessages.FALLBACK_ERR_MESSAGE,
        ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null);
  }

  @Override
  public GdnBaseRestResponse updateAssignedTo(ChangeAssigneeRequest request) {
    return new GdnBaseRestResponse(ErrorMessages.FALLBACK_ERR_MESSAGE,
        ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null);
  }

  @Override
  public GdnBaseRestResponse fetchDataFromBigQuery(String processName,
      BigQueryFetchRequest bigQueryFetchRequest) {
    return new GdnBaseRestResponse(ErrorMessages.FALLBACK_ERR_MESSAGE,
        ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null);
  }

  @Override
  public GdnRestSingleResponse<ItemSkuAndIndicativePriceResponse> fetchIndicativePrice(
    ItemSkuListRequest request) {
    return new GdnRestSingleResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE,
      ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, null);
  }
}
