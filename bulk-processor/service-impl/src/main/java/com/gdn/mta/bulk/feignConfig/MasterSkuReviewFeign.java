package com.gdn.mta.bulk.feignConfig;

import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestBody;

import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.mta.bulk.models.download.ChangeAssigneeRequest;
import com.gdn.mta.bulk.models.download.ClusterReviewFeedbackRequest;
import com.gdn.mta.bulk.models.download.DownloadItemsRequest;
import com.gdn.mta.bulk.models.download.responsedata.ClusterActionResponse;
import com.gdn.mta.bulk.models.download.responsedata.InReviewAnchorDownloadResponse;
import com.gdn.mta.bulk.models.download.responsedata.ItemsDownloadResponse;
import com.gdn.mta.bulk.models.download.DownloadInReviewAnchorsWebRequest;
import feign.Headers;
import feign.Param;
import feign.RequestLine;

public interface MasterSkuReviewFeign {

  @RequestLine(value = "POST /api/download-master-sku/download-in-review?storeId={storeId"
      + "}&channelId={channelId}&clientId={clientId}&requestId={requestId}&username={username}&page"
      + "={page}&size={size}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestListResponse<InReviewAnchorDownloadResponse> getInReviewAnchorsDownload(
    @Param("storeId") String storeId, @Param("channelId") String channelId,
    @Param("clientId") String clientId, @Param("requestId") String requestId,
    @Param("username") String username, @Param("page") int page, @Param("size") int size,
    DownloadInReviewAnchorsWebRequest request);

  @RequestLine("POST /api/products/download-items?storeId={storeId}&requestId={requestId}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestSingleResponse<ItemsDownloadResponse> downloadItemsForMasterSkuReview(@Param("storeId") String storeId,
      @Param("requestId") String requestId, DownloadItemsRequest request);

  @RequestLine(value = "POST /api/master-sku/change-assignee?storeId={storeId"
    + "}&channelId={channelId}&clientId={clientId}&requestId={requestId}&username={username}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnBaseRestResponse updateAssignedTo(@Param("storeId") String storeId,
    @Param("channelId") String channelId, @Param("clientId") String clientId,
    @Param("requestId") String requestId, @Param("username") String username,
    ChangeAssigneeRequest request);

  @RequestLine(value = "PUT /api/master-sku/{masterSku}/clusterAction?storeId={storeId}"
      + "&channelId={channelId}&clientId={clientId}&requestId={requestId}&username={username}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestSingleResponse<ClusterActionResponse> performClusterReviewAction(@Param("storeId") String storeId,
      @Param("channelId") String channelId, @Param("clientId") String clientId, @Param("requestId") String requestId,
      @Param("username") String username, @Param("masterSku") String masterItemSku,
      @RequestBody ClusterReviewFeedbackRequest request);
}
