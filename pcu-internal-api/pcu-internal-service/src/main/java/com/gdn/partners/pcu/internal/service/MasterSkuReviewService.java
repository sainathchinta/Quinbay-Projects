package com.gdn.partners.pcu.internal.service;

import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
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
import com.gdn.partners.pcu.internal.web.model.request.MasterSkuItemsDownloadWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.MasterSkuInReviewDownloadWebRequest;
import org.springframework.web.multipart.MultipartFile;

import java.util.List;

/**
 * @author Navya Naveli
 */

public interface MasterSkuReviewService {

  /**
   * Api to fetch the list of anchor mappings for in-review page
   * @param page
   * @param size
   * @param inReviewListWebRequest
   * @return list of anchor mappings in paginated format on the basis of search keyword or filter
   *         applied like category, mapped date or assigned to.
   */
  GdnRestListResponse<InReviewListResponse> getInReviewProductList(int page, int size,
      InReviewListWebRequest inReviewListWebRequest);

  /**
   * Api to fetch all master sku items list
   *
   * @param page PAGE
   * @param size SIZE
   * @param masterSkuItemsListWebRequest filter request
   * @return  GdnRestListResponse<ItemsListingResponse
   */
  GdnRestListResponse<ItemsListingResponse> getAllMasterSkuItemsList(int page, int size,
      MasterSkuItemsListWebRequest masterSkuItemsListWebRequest);

  /**
   * to fetch master sku details
   *
   * @param masterSku masterSku
   * @param allData All data flag
   * @return ItemSkuDetailResponse
   */
  GdnRestSingleResponse<ItemSkuDetailResponse> getMasterSkuDetails(String masterSku,
    boolean allData);

  /**
   * API to fetch compare anchor details
   *
   * @param firstAnchor
   * @param secondAnchor
   * @return compare anchor response
   */
  GdnRestSingleResponse<CompareAnchorResponse> getCompareAnchorDetails(String firstAnchor,
    String secondAnchor);

  /**
   * to fetch items mapped to master sku
   *
   * @param page      page
   * @param size      size
   * @param masterSku masterSku
   * @return GdnRestListResponse<ItemSkuDetailResponse>
   */
  GdnRestListResponse<ItemSkuDetailResponse> fetchItemsMappedToMasterSku(int page, int size, String masterSku);

  /**
   * to fetch item history details
   *
   * @param page    page
   * @param size    size
   * @param itemSku itemSku
   * @return GdnRestListResponse<ItemHistoryResponse>
   */
  GdnRestListResponse<ItemHistoryResponse> fetchItemHistoryDetails(int page, int size, String itemSku);

  /**
   * to fetch master sku review config
   *
   * @return GdnRestSingleResponse
   */
  GdnRestSingleResponse<MasterSkuConfigResponse> fetchMasterSkuReviewConfig();

  /**
   * To perform cluster  review action
   *
   * @param masterSku                    masterSku
   * @param clusterReviewFeedbackRequest clusterReviewFeedbackRequest
   * @return GdnBaseRestResponse
   */
  GdnRestSingleResponse<ClusterActionResponse> performClusterAction(String masterSku,
    ClusterReviewFeedbackRequest clusterReviewFeedbackRequest);

  /**
   * to update anchor mapping for accept reject action
   * @param firstAnchorSku
   * @param secondAnchorSku
   * @param request
   * @return BaseResponse
   */
  void updateAcceptRejectActionRequest (String firstAnchorSku, String secondAnchorSku, AcceptRejectActionRequest request);

  /**
   * to update anchor mapping for change assignee action
   *
   * @param request
   */
  void updateChangeAssigneeAction(ChangeAssigneeRequest request) throws Exception;

  /**
   * Get master sku reviewer list
   * @return User-name list
   */
  List<String> getMasterSkuReviewReviewers() throws Exception;

  /**
   * download items for master sku review
   *
   * @param username
   * @param request
   * @param page
   * @param size
   */
  void downloadItemsForMasterSkuReview(String username, MasterSkuItemsDownloadWebRequest request,
    int page, int size);

  /**
   * to download all or selected anchor mappings from in-review tab
   *
   * @param request
   * @param page
   * @param size
   */
  void bulkDownloadInReviewAnchorMappings(String username,
    MasterSkuInReviewDownloadWebRequest request, int page, int size);

  /**
   * Uploads bulk file on action type
   * @param file File to upload
   * @param actionType Action Type
   * @param requestId Request Id
   * @param storeId Store id
   * @param username Username
   */
  void uploadBulkFile(MultipartFile file, String actionType, String requestId, String storeId,
    String username) throws Exception;

  /**
   * fetch indicative price
   *
   * @param request ItemSkuListRequest
   * @return ItemSkuAndIndicativePriceResponse
   */
  ItemSkuAndIndicativePriceResponse fetchIndicativePrice(ItemSkuListRequest request);
}
