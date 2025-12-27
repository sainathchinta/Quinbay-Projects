package com.gdn.mta.bulk.feignConfig;

import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.mta.bulk.models.AutoApprovedAssigneeRequest;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.mta.bulk.models.AutoApprovedSelectedDownloadRequest;
import com.gdn.mta.bulk.models.download.AutoApprovedWebRequest;
import com.gdn.mta.bulk.models.download.responsedata.AutoApprovedListWebResponse;
import com.gdn.mta.bulk.models.download.responsedata.ProductAssigneeChangeResponse;
import feign.Headers;
import feign.Param;
import feign.RequestLine;

public interface ProductAnalyticsFeign {

  @RequestLine(value = "POST /product-analytics/api/auto-approved/update-assignee?storeId={storeId"
    + "}&channelId={channelId}&clientId={clientId}&requestId={requestId}&username={username}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestListResponse<ProductAssigneeChangeResponse> updateAssignedTo(
    @Param("storeId") String storeId, @Param("channelId") String channelId,
    @Param("clientId") String clientId, @Param("requestId") String requestId,
    @Param("username") String username, AutoApprovedAssigneeRequest autoApprovedAssigneeRequest);
  @RequestLine(value = "POST /product-analytics/api/auto-approved/products-list?storeId={storeId"
      + "}&channelId={channelId}&clientId={clientId}&requestId={requestId}&username={username}&page"
      + "={page}&size={size}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestListResponse<AutoApprovedListWebResponse> fetchAutoApprovedProductsList(
      @Param("storeId") String storeId, @Param("channelId") String channelId,
      @Param("clientId") String clientId, @Param("requestId") String requestId,
      @Param("username") String username, @Param("page") int page, @Param("size") int size,
      AutoApprovedWebRequest request);

  @RequestLine(value = "POST /product-analytics/api/auto-approved/selected-download?storeId={storeId"
      + "}&channelId={channelId}&clientId={clientId}&requestId={requestId}&username={username}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestListResponse<AutoApprovedListWebResponse> fetchAutoApprovedProductsSelectedDownload(
      @Param("storeId") String storeId, @Param("channelId") String channelId,
      @Param("clientId") String clientId, @Param("requestId") String requestId,
      @Param("username") String username, AutoApprovedSelectedDownloadRequest request);
}
