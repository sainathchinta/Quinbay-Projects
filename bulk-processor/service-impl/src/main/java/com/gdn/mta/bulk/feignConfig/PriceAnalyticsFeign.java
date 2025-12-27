package com.gdn.mta.bulk.feignConfig;

import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.mta.bulk.dto.TaggedProductFilterRequest;
import com.gdn.mta.bulk.dto.TaggedProductFilterResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.mta.bulk.models.FailedRebateResponse;
import com.gdn.mta.bulk.models.SkuRebateUpdateRequest;
import com.gdn.mta.bulk.models.SkuResponse;
import com.gdn.mta.bulk.models.download.BulkPriceRecommendationDownloadRequest;
import com.gdn.mta.bulk.models.download.responsedata.BulkRebateResponse;
import com.gdn.mta.bulk.models.download.responsedata.BulkRebateUpdateRequest;
import com.gdn.mta.bulk.models.download.UpdateRemoveProductTaggingRequest;
import com.gdn.mta.bulk.models.download.responsedata.DownloadSkuResponse;
import com.gdn.mta.bulk.models.download.responsedata.FailedReasonResponse;
import feign.Headers;
import feign.Param;
import feign.RequestLine;
import java.util.List;
import java.util.Set;

public interface PriceAnalyticsFeign {

  @RequestLine(value = "POST api/product/download-skus?storeId={storeId}&clientId={clientId}&channelId={channelId}&username={username}&requestId={requestId}&brEmailAddress={brEmailAddress}&page={page}&size={size}")
  @Headers({"Content-Type: application/json", "accept: application/json"})
  GdnRestListResponse<DownloadSkuResponse> getSkusForDownload(@Param("storeId") String storeId,
      @Param("clientId") String clientId, @Param("channelId") String channelId, @Param("username") String username,
      @Param("requestId") String requestId, @Param("brEmailAddress") String brEmailAddress, @Param("page") int page,
      @Param("size") int size, BulkPriceRecommendationDownloadRequest bulkPriceRecommendationDownloadRequest);

  @RequestLine(value = "POST api/product/getTaggedProducts?storeId={storeId}&clientId={clientId}&channelId={channelId}&username={username}&requestId={requestId}&page={page}&size={size}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestListResponse<TaggedProductFilterResponse> getTaggedProducts(@Param("storeId") String storeId,
      @Param("clientId") String clientId, @Param("channelId") String channelId, @Param("username") String username,
      @Param("requestId") String requestId, @Param("page") int page, @Param("size") int size,
      TaggedProductFilterRequest taggedProductFilterRequest);

  @RequestLine(value = "POST /api/rebate/update?storeId={storeId}&clientId={clientId}&channelId={channelId}&username={username}&requestId={requestId}&officerEmailAddress={brEmailAddress}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestSingleResponse<BulkRebateResponse> updateRebate(@Param("storeId") String storeId, @Param("clientId") String clientId,
      @Param("channelId") String channelId, @Param("username") String username, @Param("requestId") String requestId,
      @Param("brEmailAddress") String brEmailAddress, BulkRebateUpdateRequest bulkRebateUpdateRequest);

  @RequestLine("POST /api/product/updateTagging?storeId={storeId}&clientId={clientId}&channelId={channelId}&username={username}&requestId={requestId}&officerEmailAddress={officerEmailAddress}")
  @Headers("Content-Type: application/json")
  GdnRestListResponse<FailedReasonResponse> updateTagging(@Param("storeId") String storeId,
    @Param("clientId") String clientId,
    @Param("channelId") String channelId, @Param("username") String username,
    @Param("requestId") String requestId, @Param("officerEmailAddress") String officerEmailAddress,
    List<UpdateRemoveProductTaggingRequest> updateRemoveProductTaggingRequestList);


  @RequestLine("POST /api/product/removeTagging?storeId={storeId}&clientId={clientId}&channelId={channelId}&username={username}&requestId={requestId}&officerEmailAddress={officerEmailAddress}")
  @Headers({"Content-Type: application/json", "accept: application/json"})
  GdnRestListResponse<FailedReasonResponse> removeTagging(@Param("storeId") String storeId, @Param("clientId") String clientId,
    @Param("channelId") String channelId, @Param("username") String username,
    @Param("requestId") String requestId, @Param("officerEmailAddress") String officerEmailAddress,
    List<UpdateRemoveProductTaggingRequest> removeTaggingRequestList);

  @RequestLine(value = "POST api/product/rebate/update?storeId={storeId}&clientId={clientId}&channelId={channelId}&username={username}&requestId={requestId}")
  @Headers({"Content-Type: application/json", "accept: application/json"})
  GdnRestSingleResponse<FailedRebateResponse> updateSkuRebate(@Param("storeId") String storeId,
    @Param("clientId") String clientId, @Param("channelId") String channelId, @Param("username") String username,
    @Param("requestId") String requestId, SkuRebateUpdateRequest skuRebateUpdateRequest);

  @RequestLine(value = "GET /api/product/getOfficerTaggedSkus?storeId={storeId}&clientId={clientId}&channelId={channelId}&username={username}&requestId={requestId}&itemPickupPointIds={itemPickupPointIds}")
  @Headers({"Content-Type: application/json", "accept: application/json"})
  GdnRestListResponse<SkuResponse> getOfficerTaggedSkus(@Param("storeId") String storeId,
    @Param("clientId") String clientId, @Param("channelId") String channelId, @Param("username") String username,
    @Param("requestId") String requestId, @Param("itemPickupPointIds") Set<String> itemPickupPointIds);
}
