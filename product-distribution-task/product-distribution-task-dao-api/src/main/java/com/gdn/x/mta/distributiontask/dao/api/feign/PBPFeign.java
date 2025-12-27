package com.gdn.x.mta.distributiontask.dao.api.feign;

import java.util.List;

import com.gda.mta.product.dto.AutoApprovalTypeRequest;
import com.gda.mta.product.dto.AutoQcConfigChangeRequest;
import com.gda.mta.product.dto.DeleteProductRequest;
import com.gda.mta.product.dto.ImageQcProcessedAndBrandResponse;
import com.gda.mta.product.dto.ProductHistoryRequest;
import com.gda.mta.product.dto.ProductSystemParameterResponse;
import com.gda.mta.product.dto.RetryNeedRevisionRequest;
import com.gda.mta.product.dto.SuspensionProductRequest;
import com.gda.mta.product.dto.response.AutoApprovalTypeResponse;
import com.gda.mta.product.dto.response.SimpleStringResponse;
import com.gda.mta.product.dto.response.RetryAutoNeedRevisionResponse;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.mta.product.util.SingleValueResponse;
import com.gdn.partners.pbp.dto.workflow.product.ProductWorkflowStatusResponse;
import com.gdn.x.mta.distributiontask.model.dto.ProductCodeAndSkuRequest;
import com.gdn.x.mta.distributiontask.model.dto.ProductSkuDetailResponse;
import com.gdn.x.productcategorybase.dto.response.ProductDetailResponse;

import feign.Headers;
import feign.Param;
import feign.RequestLine;

public interface PBPFeign {

  @RequestLine("PUT /api/product/post-live/{productCode}?storeId={storeId}&channelId={channelId}&clientId={clientId}&requestId={requestId}&username={username}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnBaseRestResponse updateProductAsPostLive(
      @Param("storeId") String storeId, @Param("channelId") String channelId,
      @Param("clientId") String clientId, @Param("requestId") String requestId,
      @Param("username") String username, @Param("productCode") String productCode);

  @RequestLine("GET /api/productImagePrediction/getImageQcPredictionAndBrandResponse?storeId={storeId}&channelId={channelId}&clientId={clientId}&requestId={requestId}&username={username}&productCode={productCode}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestSingleResponse<ImageQcProcessedAndBrandResponse> getImageQcPredictionAndBrandResponse(
      @Param("storeId") String storeId, @Param("channelId") String channelId, @Param("clientId") String clientId,
      @Param("requestId") String requestId, @Param("username") String username,
      @Param("productCode") String productCode);

  @RequestLine("GET /api/productSystemParameter/find?storeId={storeId}&channelId={channelId}&clientId={clientId}&requestId={requestId}&variable={variable}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestSingleResponse<ProductSystemParameterResponse> findSystemParameter(@Param("storeId") String storeId,
      @Param("channelId") String channelId, @Param("clientId") String clientId, @Param("requestId") String requestId,
      @Param("variable") String variable);

  @RequestLine("PUT /api/product/{productCode}/updateReviewType/{reviewType}?storeId={storeId}&channelId={channelId"
                   + "}&clientId={clientId}&requestId={requestId}&username={username}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnBaseRestResponse updateReviewType(@Param("storeId") String storeId, @Param("channelId") String channelId,
      @Param("clientId") String clientId, @Param("requestId") String requestId, @Param("username") String username,
      @Param("productCode") String productCode, @Param("reviewType") String reviewType);

  @RequestLine("POST /api/product/{productCode}/check-auto-approval-eligibility?storeId={storeId}&channelId={channelId"
      + "}&clientId={clientId}&requestId={requestId}&username={username}&onlyCategoryChange={onlyCategoryChange}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestSingleResponse<AutoApprovalTypeResponse> getAutoApprovalType(@Param("storeId") String storeId,
      @Param("channelId") String channelId, @Param("clientId") String clientId, @Param("requestId") String requestId,
      @Param("username") String username, @Param("onlyCategoryChange") boolean onlyCategoryChange,
      @Param("productCode") String productCode, AutoApprovalTypeRequest autoApprovalTypeRequest);

  @RequestLine("PUT /api/product/retry-auto-need-revision?storeId={storeId}&channelId={channelId"
      + "}&clientId={clientId}&requestId={requestId}&username={username}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestSingleResponse<RetryAutoNeedRevisionResponse> retryAutoNeedRevision(@Param("storeId") String storeId,
      @Param("channelId") String channelId, @Param("clientId") String clientId,
      @Param("requestId") String requestId, @Param("username") String username,
      RetryNeedRevisionRequest retryNeedRevisionRequest);

  @RequestLine("GET /api/product/{productCode}/get-product-status?storeId={storeId}&channelId={channelId"
      + "}&clientId={clientId}&requestId={requestId}&username={username}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestSingleResponse<SimpleStringResponse> getProductStatus(@Param("storeId") String storeId,
      @Param("channelId") String channelId, @Param("clientId") String clientId, @Param("requestId") String requestId,
      @Param("username") String username, @Param("productCode") String productCode);

  @RequestLine("POST /api/auto-qc-config/verify-rule-config-change?storeId={storeId}&channelId={channelId}&clientId={clientId}&requestId={requestId}&username={username}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestSingleResponse<SingleValueResponse> verifyAutoQcConfigChange(@Param("storeId") String storeId,
      @Param("channelId") String channelId, @Param("clientId") String clientId, @Param("requestId") String requestId,
      @Param("username") String username, AutoQcConfigChangeRequest autoQcConfigChangeRequest);

  @RequestLine("GET /api/product-workflow/status?storeId={storeId}&channelId={channelId"
      + "}&clientId={clientId}&requestId={requestId}&username={username}&productCode={productCode}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestSingleResponse<ProductWorkflowStatusResponse> getProductWorkflowStatus(@Param("storeId") String storeId,
      @Param("channelId") String channelId, @Param("clientId") String clientId, @Param("requestId") String requestId,
      @Param("username") String username, @Param("productCode") String productCode);

  @RequestLine("POST /api/product/delete-product-collection?storeId={storeId}&channelId={channelId"
      + "}&clientId={clientId}&requestId={requestId}&username={username}&needEmailNotification={needEmailNotification}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnBaseRestResponse deleteProductCollection(@Param("storeId") String storeId, @Param("channelId") String channelId,
      @Param("clientId") String clientId, @Param("requestId") String requestId, @Param("username") String username,
      @Param("needEmailNotification") boolean needEmailNotification, DeleteProductRequest deleteProductRequest);

  @RequestLine("POST /api/product/product-details-by-product-codes?storeId={storeId}&channelId={channelId"
      + "}&clientId={clientId}&requestId={requestId}&username={username}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestListResponse<ProductDetailResponse> getProductDetailsByProductCodes(@Param("storeId") String storeId,
      @Param("channelId") String channelId, @Param("clientId") String clientId, @Param("requestId") String requestId,
      @Param("username") String username, List<String> productCodeList);

  @RequestLine("GET /api/product/publishProductToPDT?storeId={storeId}&channelId={channelId"
      + "}&clientId={clientId}&requestId={requestId}&username={username}&productCode={productCode}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnBaseRestResponse publishProductToPDTByProductCode(@Param("storeId") String storeId,
      @Param("channelId") String channelId, @Param("clientId") String clientId, @Param("requestId") String requestId,
      @Param("username") String username, @Param("productCode") String productCode);

  @RequestLine("POST /api/product/submit-history?storeId={storeId}&channelId={channelId"
      + "}&clientId={clientId}&requestId={requestId}&username={username}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnBaseRestResponse submitHistory(@Param("storeId") String storeId, @Param("channelId") String channelId,
      @Param("clientId") String clientId, @Param("requestId") String requestId, @Param("username") String username,
      ProductHistoryRequest productHistoryRequest);

  @RequestLine(
      "GET /api/product/{productCode}/process-product-vendor-search-auto-heal?storeId={storeId}&channelId={channelId"
          + "}&clientId={clientId}&requestId={requestId}&username={username}&productCode={productCode}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnBaseRestResponse processProductVendorSearchAutoHeal(@Param("storeId") String storeId,
      @Param("channelId") String channelId, @Param("clientId") String clientId, @Param("requestId") String requestId,
      @Param("username") String username, @Param("productCode") String productCode);

  @RequestLine("POST /api/product-level3/getProductSkuDetail?storeId={storeId}&channelId={channelId"
      + "}&clientId={clientId}&requestId={requestId}&username={username}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestSingleResponse<ProductSkuDetailResponse> getProductSkuDetailResponse(
      @Param("storeId") String storeId, @Param("channelId") String channelId,
      @Param("clientId") String clientId, @Param("requestId") String requestId,
      @Param("username") String username, ProductCodeAndSkuRequest request);

  @RequestLine("POST /api/product-level3/doSuspensionAction?storeId={storeId}&channelId={channelId"
      + "}&clientId={clientId}&requestId={requestId}&username={username}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnBaseRestResponse doSuspensionAction(@Param("storeId") String storeId,
      @Param("channelId") String channelId, @Param("clientId") String clientId,
      @Param("requestId") String requestId, @Param("username") String username,
      SuspensionProductRequest request);
}
