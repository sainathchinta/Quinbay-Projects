package com.gdn.partners.pbp.outbound.pdt.feign;

import com.gda.mta.product.dto.AppealProductRequest;
import com.gda.mta.product.dto.AppealProductResponse;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.x.mta.distributiontask.request.ProductRetryStatusUpdate;
import com.gdn.x.mta.distributiontask.request.RemoveProductRequest;
import com.gdn.x.mta.distributiontask.response.ProductImageQcFeedbackResponse;
import com.gdn.x.mta.distributiontask.rest.model.request.AutoNeedRevisionRequest;
import com.gdn.x.mta.distributiontask.rest.model.request.ChangeBrandRequest;
import com.gdn.x.mta.distributiontask.rest.model.response.DistributionProductDetailResponse;
import com.gdn.x.mta.distributiontask.rest.model.response.PDTProductDomainEventModelResponse;
import com.gdn.x.mta.distributiontask.util.GdnRestSimpleResponse;
import feign.Headers;
import feign.Param;
import feign.RequestLine;

public interface PDTFeign {

  @RequestLine("GET /product/product-existence?storeId={storeId}&channelId={channelId}&clientId"
      + "={clientId}&requestId={requestId}&username={username}&productCode={productCode}&allProducts={allProducts}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestSimpleResponse<Boolean> checkIfProductExistsInPdt(@Param("storeId") String storeId,
      @Param("channelId") String channelId, @Param("clientId") String clientId, @Param("requestId") String requestId,
      @Param("username") String username, @Param("productCode") String productCode,
      @Param("allProducts") boolean allProducts);

  @RequestLine("GET /vendor/image-feedback/{productCode}?storeId={storeId}&channelId={channelId}&clientId"
      + "={clientId}&requestId={requestId}&username={username}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestSimpleResponse<ProductImageQcFeedbackResponse> getProductImageFeedback(@Param("storeId") String storeId,
      @Param("channelId") String channelId, @Param("clientId") String clientId, @Param("requestId") String requestId,
      @Param("username") String username, @Param("productCode") String productCode);

  @RequestLine("GET /product/sendProductBackToVendor?storeId={storeId}&channelId={channelId}&clientId"
      + "={clientId}&requestId={requestId}&username={username}&productCode={productCode}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnBaseRestResponse sendProductBackToVendor(@Param("storeId") String storeId,
      @Param("channelId") String channelId, @Param("clientId") String clientId, @Param("requestId") String requestId,
      @Param("username") String username, @Param("productCode") String productCode);

  @RequestLine("POST /product/sendProductToAutoNeedRevision?storeId={storeId}&channelId={channelId}&clientId"
      + "={clientId}&requestId={requestId}&username={username}&validateAssignment={validateAssignment}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnBaseRestResponse sendProductToAutoNeedRevision(@Param("storeId") String storeId,
      @Param("channelId") String channelId, @Param("clientId") String clientId, @Param("requestId") String requestId,
      @Param("username") String username, @Param("validateAssignment") boolean validateAssignment,
      AutoNeedRevisionRequest autoNeedRevisionRequest);

  @RequestLine("POST /product/{productCode}/product-retry-status-update?storeId={storeId}&channelId={channelId}&clientId"
      + "={clientId}&requestId={requestId}&username={username}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnBaseRestResponse productRetryStatusUpdate(@Param("storeId") String storeId,
      @Param("channelId") String channelId, @Param("clientId") String clientId,
      @Param("requestId") String requestId, @Param("username") String username,
      @Param("productCode") String productCode, ProductRetryStatusUpdate productRetryStatusUpdate);

  @RequestLine("POST /product/updateProductBrand?storeId={storeId}&channelId={channelId}&clientId"
      + "={clientId}&requestId={requestId}&username={username}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnBaseRestResponse updateProductBrand(@Param("storeId") String storeId, @Param("channelId") String channelId,
      @Param("clientId") String clientId, @Param("requestId") String requestId, @Param("username") String username,
      ChangeBrandRequest changeBrandRequest);

  @RequestLine("POST /product/appeal-product?storeId={storeId}&channelId={channelId}&clientId"
    + "={clientId}&requestId={requestId}&username={username}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestSingleResponse<AppealProductResponse> updateAppealProduct(@Param("storeId") String storeId,
    @Param("channelId") String channelId, @Param("clientId") String clientId,
    @Param("requestId") String requestId, @Param("username") String username,
    AppealProductRequest appealProductRequest);

  @RequestLine("POST /qcTask/move-failed-product-to-qc?storeId={storeId}&channelId={channelId"
      + "}&clientId={clientId}&requestId={requestId}&username={username}&productCode={productCode}")
  @Headers("Accept: application/json")
  GdnBaseRestResponse moveFailedProductToQC(@Param("storeId") String storeId,
      @Param("channelId") String channelId, @Param("clientId") String clientId,
      @Param("requestId") String requestId, @Param("username") String username,
      @Param("productCode") String productCode);

  @RequestLine("GET /product/details-for-all-product-type?storeId={storeId}&channelId={channelId"
      + "}&clientId={clientId}&requestId={requestId}&username={username}&productCode={productCode}")
  @Headers("Accept: application/json")
  GdnRestSingleResponse<DistributionProductDetailResponse> getDetailsForProduct(
      @Param("storeId") String storeId, @Param("channelId") String channelId,
      @Param("clientId") String clientId, @Param("requestId") String requestId,
      @Param("username") String username, @Param("productCode") String productCode);

  @RequestLine("POST /distirbutionTask/remove-product?storeId={storeId}&channelId={channelId"
      + "}&clientId={clientId}&requestId={requestId}&username={username}")
  @Headers({"Accept: application/json", "Content-Type: application/json"})
  GdnBaseRestResponse removeProductFromPDT(@Param("storeId") String storeId,
      @Param("channelId") String channelId, @Param("clientId") String clientId,
      @Param("requestId") String requestId, @Param("username") String username,
      RemoveProductRequest removeProductRequest);


  @RequestLine(
      "GET /product/get-product-domain-response-by-code?storeId={storeId}&channelId={channelId"
          + "}&clientId={clientId}&requestId={requestId}&username={username}&productCode"
          + "={productCode}")
  @Headers("Accept: application/json")
  GdnRestSingleResponse<PDTProductDomainEventModelResponse> getPDTDomainModelResponseByCode(
      @Param("storeId") String storeId, @Param("channelId") String channelId,
      @Param("clientId") String clientId, @Param("requestId") String requestId,
      @Param("username") String username, @Param("productCode") String productCode);

}
