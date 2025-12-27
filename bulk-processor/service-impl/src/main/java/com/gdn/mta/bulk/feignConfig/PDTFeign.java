package com.gdn.mta.bulk.feignConfig;

import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.mta.bulk.models.BrandReport;
import com.gdn.mta.bulk.models.IprActionRequest;
import com.gdn.mta.bulk.models.download.IPRProductListRequest;
import com.gdn.mta.bulk.models.download.responsedata.DistributionProductResponse;
import com.gdn.mta.bulk.models.download.responsedata.IprProductsResponse;
import com.gdn.x.mta.distributiontask.response.ProductCodeResponse;
import com.gdn.x.mta.distributiontask.rest.model.request.BoostedProductFilterRequest;
import com.gdn.x.mta.distributiontask.rest.model.request.BulkVendorProductActionsRequest;
import com.gdn.x.mta.distributiontask.rest.model.request.FilterSummaryRequest;
import com.gdn.x.mta.distributiontask.rest.model.request.ProductListRequest;
import com.gdn.x.mta.distributiontask.rest.model.request.RejectProductVendorRequest;
import com.gdn.x.mta.distributiontask.rest.model.request.VendorQuickApprovalRequest;
import com.gdn.x.mta.distributiontask.rest.model.response.BulkVendorProductActionsResponse;
import com.gdn.x.mta.distributiontask.rest.model.response.VendorQuickApprovalResponse;
import feign.Headers;
import feign.Param;
import feign.RequestLine;
import org.springframework.web.bind.annotation.RequestBody;

public interface PDTFeign {

  @RequestLine("POST /vendor-activity/reject-product?"
    + "storeId={storeId}&channelId={channelId}&clientId={clientId}"
    + "&requestId={requestId}&username={username}&vendorCode={vendorCode}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnBaseRestResponse vendorRejection(@Param("storeId") String storeId,
    @Param("channelId") String channelId, @Param("clientId") String clientId, @Param("requestId") String requestId,
    @Param("username") String username, @Param("vendorCode") String vendorCode, RejectProductVendorRequest rejectProductVendorRequest);

  @RequestLine("POST /vendor-activity/quick-approval?"
    + "storeId={storeId}&channelId={channelId}&clientId={clientId}"
    + "&requestId={requestId}&username={username}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestSingleResponse<VendorQuickApprovalResponse> vendorQuickApproval(@Param("storeId") String storeId,
    @Param("channelId") String channelId, @Param("clientId") String clientId, @Param("requestId") String requestId,
    @Param("username") String username, VendorQuickApprovalRequest vendorQuickApprovalRequest);

  @RequestLine("POST /api/ipr/action?storeId={storeId}&channelId={channelId}&clientId={clientId}"
      + "&requestId={requestId}&username={username}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnBaseRestResponse performIprAction(@Param("storeId") String storeId,
      @Param("channelId") String channelId, @Param("clientId") String clientId,
      @Param("requestId") String requestId, @Param("username") String username,
      IprActionRequest iprActionRequest);

  @RequestLine("POST /api/ipr/add-product/{productSku}?storeId={storeId}&channelId={channelId"
      + "}&clientId={clientId}&requestId={requestId}&username={username}&source={source}&assignee={assignee}")
  @Headers("Content-Type: application/json")
  GdnBaseRestResponse addProductToIPR(@Param("storeId") String storeId,
      @Param("channelId") String channelId, @Param("clientId") String clientId,
      @Param("requestId") String requestId, @Param("username") String username,
      @Param("productSku") String productSku, @Param("source") String source,
      @Param("assignee") String assignee, @RequestBody(required = false)BrandReport brandReport);

  @RequestLine("POST /api/ipr/filter/summary?storeId={storeId}&channelId={channelId}&clientId"
      + "={clientId}&requestId={requestId}&username={username}&page={page}&size={size}")
  @Headers("Content-Type: application/json")
  GdnRestListResponse<IprProductsResponse> getIprProductsList(
      @Param("storeId") String storeId, @Param("requestId") String requestId, @Param("channelId") String channelId,
      @Param("clientId") String clientId, @Param("username") String username,
      IPRProductListRequest request, @Param("page") int page,
      @Param("size") int size);

  @RequestLine("POST /product/filter/summary?storeId={storeId}&channelId={channelId}&clientId={clientId}&requestId={requestId}&username={username}&page={page"
      + "}&size={size}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestListResponse<DistributionProductResponse> getProductList(
      @Param("storeId") String storeId, @Param("requestId") String requestId,
      @Param("channelId") String channelId, @Param("clientId") String clientId,
      @Param("username") String username, @Param("page") int page,
      @Param("size") int size, FilterSummaryRequest filterSummaryRequest)
      throws Exception;


  @RequestLine("POST /product/getDetailsByVendor?storeId={storeId}&channelId={channelId}&clientId={clientId}&requestId={requestId}&username={username}&page={page"
      + "}&size={size}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestListResponse<DistributionProductResponse> getProductListingForVendorCode(@Param("storeId") String storeId, @Param("requestId") String requestId,
      @Param("channelId") String channelId, @Param("clientId") String clientId,
      @Param("username") String username, @Param("page") int page,
      @Param("size") int size, ProductListRequest request);

  @RequestLine("POST /vendor-activity/bulkVendorProductActions?storeId={storeId}&channelId"
      + "={channelId}&clientId={clientId}&requestId={requestId}&username={username}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestSingleResponse<BulkVendorProductActionsResponse> bulkVendorProductActions(
      @Param("storeId") String storeId, @Param("requestId") String requestId,
      @Param("channelId") String channelId, @Param("clientId") String clientId,
      @Param("username") String username, BulkVendorProductActionsRequest request) throws Exception;

  @RequestLine("POST /product/fetchProductsForAutoAssignment?storeId={storeId}&channelId"
      + "={channelId}&clientId={clientId}&requestId={requestId}&username={username}&page={page"
      + "}&size={size}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestListResponse<ProductCodeResponse> fetchProductsForAutoAssignment(
      @Param("storeId") String storeId, @Param("requestId") String requestId,
      @Param("channelId") String channelId, @Param("clientId") String clientId,
      @Param("username") String username, @Param("page") Integer page, @Param("size") Integer size,
      BoostedProductFilterRequest request) throws Exception;

}
