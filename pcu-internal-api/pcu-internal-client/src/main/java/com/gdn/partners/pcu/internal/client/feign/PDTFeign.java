package com.gdn.partners.pcu.internal.client.feign;

import com.gdn.partners.pcu.internal.client.model.request.IPRProductListRequest;
import com.gdn.partners.pcu.internal.client.model.request.IprActionRequest;
import com.gdn.partners.pcu.internal.client.model.response.DistributionProductResponse;
import com.gdn.partners.pcu.internal.client.model.response.IPRProductDetailResponse;
import com.gdn.partners.pcu.internal.client.model.request.IPRUpdateAssigneeRequest;
import com.gdn.partners.pcu.internal.client.model.response.IPRProductHistoryResponse;
import com.gdn.partners.pcu.internal.client.model.response.IPRProductListResponse;
import com.gdn.partners.pcu.internal.client.model.response.IprSuspensionInProgressResponse;
import com.gdn.partners.pcu.internal.client.model.response.SystemParameterResponse;
import com.gdn.x.mta.distributiontask.request.VendorDefaultFilterRequest;
import com.gdn.x.mta.distributiontask.response.VendorDefaultFilterResponse;

import com.gdn.x.mta.distributiontask.rest.model.request.FilterSummaryRequest;
import com.gdn.x.mta.distributiontask.rest.model.request.PrimaryFilterRequest;
import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;

import com.gda.mta.product.dto.ProductBusinessPartnerMapperResponse;
import com.gda.mta.product.dto.ScreeningProductBulkActionsRequest;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.partners.pcu.internal.client.factory.PDTFeignFallbackFactory;
import com.gdn.x.mta.distributiontask.request.DistributionTaskMultipleFilterRequest;
import com.gdn.x.mta.distributiontask.request.NeedRevisionRequest;
import com.gdn.x.mta.distributiontask.request.ProductDistributionTaskRequest;
import com.gdn.x.mta.distributiontask.request.ProductRetryStatusUpdate;
import com.gdn.x.mta.distributiontask.request.RejectProductRequest;
import com.gdn.x.mta.distributiontask.response.ProductImageQcFeedbackResponse;
import com.gdn.x.mta.distributiontask.response.VendorDetailResponse;
import com.gdn.x.mta.distributiontask.rest.model.request.DistributionProductDetailRequest;
import com.gdn.x.mta.distributiontask.rest.model.request.ProductListRequest;
import com.gdn.x.mta.distributiontask.rest.model.request.ProductImageQcFeedbackRequest;
import com.gdn.x.mta.distributiontask.rest.model.request.RejectProductVendorRequest;
import com.gdn.x.mta.distributiontask.rest.model.request.VendorQuickApprovalRequest;
import com.gdn.x.mta.distributiontask.rest.model.response.DistributionProductDetailResponse;
import com.gdn.x.mta.distributiontask.rest.model.response.MapResponse;
import com.gdn.x.mta.distributiontask.rest.model.response.NeedRevisionResponse;
import com.gdn.x.mta.distributiontask.rest.model.response.PDTProductDomainEventModelResponse;
import com.gdn.x.mta.distributiontask.rest.model.response.TaskHistoryResponse;
import com.gdn.x.mta.distributiontask.rest.model.response.VendorAssigneeResponse;
import com.gdn.x.mta.distributiontask.rest.model.response.VendorQuickApprovalResponse;
import com.gdn.x.mta.distributiontask.util.GdnRestSimpleResponse;

/**
 * Created by shivam on 26/06/2019 AD.
 */

@FeignClient(name = "pdtFeign", url = "${service.pdt.endpoint}", fallbackFactory = PDTFeignFallbackFactory.class)
public interface PDTFeign {

  @RequestMapping(value = "/product/getBusinessPartnerList", method = RequestMethod.POST)
  GdnRestListResponse<ProductBusinessPartnerMapperResponse> getBusinessPartnerList(
      @RequestParam(value = "page", defaultValue = "0", required = false) int page,
      @RequestParam(value = "size", defaultValue = "10", required = false) int size,
      @RequestBody PrimaryFilterRequest primaryFilterRequest);

  @RequestMapping(value = "/product/getAssigneeList", method = RequestMethod.POST)
  GdnRestListResponse<VendorAssigneeResponse> getAssigneeList(
      @RequestParam(value = "page", defaultValue = "0", required = false) int page,
      @RequestParam(value = "size", defaultValue = "10", required = false) int size,
      @RequestBody PrimaryFilterRequest primaryFilterRequest);

  @RequestMapping(value = "/task-history/task-history-summary-by-product-code", method = RequestMethod.GET)
  GdnRestListResponse<TaskHistoryResponse> getProductHistory(
      @RequestParam("productCode") String productCode, @RequestParam("page") int page,
      @RequestParam("size") int size, @RequestParam("escapeString") boolean escapeString);

  @RequestMapping(value = "/product/details", method = RequestMethod.GET)
  GdnRestSingleResponse<DistributionProductDetailResponse> getProductDetails(@RequestParam("requestId") String requestId,
      @RequestParam("username") String username, @RequestParam("productCode") String productCode);

  @RequestMapping(value = "/vendor-activity/update-product-content", method = RequestMethod.POST)
  GdnBaseRestResponse updateProductContent(@RequestParam("vendorCode") String vendorCode,
      @RequestBody DistributionProductDetailRequest productRequest);

  @RequestMapping(value = "/vendor-activity/update-product-image", method = RequestMethod.POST)
  GdnBaseRestResponse updateProductImage(@RequestParam("vendorCode") String vendorCode,
      @RequestBody DistributionProductDetailRequest productRequest);

  @RequestMapping(value = "/distirbutionTask/product-need-correction", method = RequestMethod.POST)
  GdnRestSingleResponse<NeedRevisionResponse> doProductNeedCorrection( @RequestParam("vendorCode") String vendorCode,
      @RequestBody NeedRevisionRequest needRevisionRequest);

  @RequestMapping(value = "/vendor-activity/vendorProductActions/{action}/action", method = RequestMethod.POST)
  GdnBaseRestResponse doVendorProductActions(
      @PathVariable("action") String action, @RequestBody ScreeningProductBulkActionsRequest request);

  @RequestMapping(value = "/vendor-activity/reject-product", method = RequestMethod.POST)
  GdnBaseRestResponse rejectProduct(@RequestParam("vendorCode") String vendorCode,
      @RequestBody RejectProductVendorRequest request);

  @RequestMapping(value = "/product/filter/summary", method = RequestMethod.POST)
  GdnRestListResponse<DistributionProductResponse> getProductList(@RequestParam("page") int page,
      @RequestParam("size") int size, @RequestBody FilterSummaryRequest summaryFilterRequest);

  @RequestMapping(value = "/product/productFilterCountInReview", method = RequestMethod.GET)
  GdnRestSingleResponse<MapResponse> getProductFilterInReview(@RequestParam("vendorCode") String vendorCode,
    @RequestParam("postLive") Boolean postLive, @RequestParam("edited") Boolean edited,
    @RequestParam("revised") Boolean revised);

  @RequestMapping(value = "/vendor-activity/approval-new", method = RequestMethod.POST)
  GdnBaseRestResponse approveVendorProduct(@RequestParam("vendorCode") String vendorCode,
      @RequestBody DistributionProductDetailRequest productRequest);

  @RequestMapping(value = "/product/sendProductBackToVendor", method = RequestMethod.GET)
  GdnBaseRestResponse sendProductBackToVendor(@RequestParam("productCode") String productCode);

  @RequestMapping(value = "/product/productReviewConfigCounts", method = RequestMethod.GET)
  GdnRestSingleResponse<MapResponse> getProductReviewConfigCounts(@RequestParam("vendorCode") String vendorCode);

  @RequestMapping(value = "/product/countDistributionSummaryByFilter", method = RequestMethod.POST)
  GdnRestSingleResponse<MapResponse> countDistributionSummaryByFilter(
      @RequestParam("includeStatus") Boolean includeStatus, @RequestParam("includeVendors") Boolean includeVendors,
      @RequestBody DistributionTaskMultipleFilterRequest request);

  @RequestMapping(value = "/qcTask/filter/product/qcready-filter-summary", method = RequestMethod.POST)
  GdnRestListResponse<DistributionProductResponse> filterProduct(@RequestParam("status") String status,
      @RequestParam("page") int page, @RequestParam("size") int size,
      @RequestBody ProductListRequest productListRequest);

  @RequestMapping(value = "/product/get-product-domain-response-by-code", method = RequestMethod.GET)
  GdnRestSingleResponse<PDTProductDomainEventModelResponse> getPDTDomainModelResponseByCode(
      @RequestParam("productCode") String productCode);

  @RequestMapping(value = "/product/getDistributionSummaryByMultipleFilter", method = RequestMethod.POST)
  GdnRestListResponse<DistributionProductResponse> getSummaryByMultipleFilter(@RequestParam("page") Integer page,
      @RequestParam("size") Integer size,  @RequestParam("sortBy") String sortBy, @RequestBody DistributionTaskMultipleFilterRequest request);

  @RequestMapping(value = "/distirbutionTask/product-vendor-mapping", method = RequestMethod.POST)
  GdnBaseRestResponse saveProductDistributionTask(@RequestBody ProductDistributionTaskRequest request);

  @RequestMapping(value = "/vendor/image-feedback/{productCode}", method = RequestMethod.GET)
  GdnRestSimpleResponse<ProductImageQcFeedbackResponse> getProductImageFeedback(
      @RequestParam("productCode") String productCode);

  @RequestMapping(value = "/vendor-activity/update/image-feedback", method = RequestMethod.PUT)
  GdnBaseRestResponse updateProductImageFeedback(
      @RequestBody ProductImageQcFeedbackRequest productImageQcFeedbackRequest);

  @RequestMapping(value = "/task-history/task-history-summary-by-product-code", method = RequestMethod.GET)
  GdnRestListResponse<TaskHistoryResponse> getProductHistories(@RequestParam("page") Integer page,
      @RequestParam("size") Integer size, @RequestParam("productCode") String productCode,
      @RequestParam("escapeString") boolean escapeString);

  @RequestMapping(value = "/product/product-existence", method = RequestMethod.GET)
  GdnRestSimpleResponse<Boolean> isProductExists(@RequestParam("productCode") String productCode);

  @RequestMapping(value = "/product/{productCode}/detect-edit-by-merchant", method = RequestMethod.GET)
  GdnRestSimpleResponse<Boolean> getEditedByMerchant(@PathVariable("productCode") String productCode,
      @RequestParam("version") int version);

  @RequestMapping(value = "/qcTask/reject-product", method = RequestMethod.POST)
  GdnBaseRestResponse rejectQCProduct(@RequestBody RejectProductRequest request);

  @RequestMapping(value = "/qcTask/approve-product", method = RequestMethod.POST)
  GdnBaseRestResponse approveQCProduct(@RequestParam("productId") String productId);

  @RequestMapping(value = "/product/filter/businessPartner", method = RequestMethod.GET)
  GdnRestListResponse<ProductBusinessPartnerMapperResponse> filterProductBusinessPartnerMapperByWorkFlowState(
      @RequestParam("page") Integer page, @RequestParam("size") Integer size,
      @RequestParam("isSearch") boolean isSearch, @RequestParam("searchCriteria") String searchCriteria,
      @RequestParam("workflowState") String workflowState);

  @RequestMapping(value = "/api/solr-index/deltaReindexPDTSolr", method = RequestMethod.GET)
  GdnBaseRestResponse deltaReindexPDTProductSolr(@RequestParam("productCode") String productCode);

  @RequestMapping(value = "/product/getReviewConfigCount", method = RequestMethod.GET)
  GdnRestSingleResponse<MapResponse> getReviewConfigCount(@RequestParam("vendorCode") String vendorCode,
      @RequestParam("postLive") boolean postLive);

  @RequestMapping(value = "/product/republish-edited-product/{productCode}", method = RequestMethod.GET)
  GdnBaseRestResponse republishEditedProduct(@PathVariable(value = "productCode") String productCode);

  @RequestMapping(value = "/vendor/get-vendor-for-code", method = RequestMethod.GET)
  GdnRestSingleResponse<VendorDetailResponse> getVendorByCode(@RequestParam("vendorCode") String vendorCode);

  @RequestMapping(value = "/vendor-activity/quick-approval", method = RequestMethod.POST)
  GdnRestSingleResponse<VendorQuickApprovalResponse> quickApproveProduct(
      @RequestBody VendorQuickApprovalRequest vendorQuickApprovalRequest);

  @RequestMapping(value = "/product/{productCode}/product-retry-status-update", method = RequestMethod.POST)
  GdnBaseRestResponse updateProductStateInPDT(@PathVariable(value = "productCode") String productCode,
      @RequestBody ProductRetryStatusUpdate productRetryStatusUpdate);

  @PostMapping(value = "/vendor/saveDefaultSettings")
  GdnBaseRestResponse saveDefaultSetting(@RequestBody VendorDefaultFilterRequest vendorDefaultFilterRequest);

  @RequestMapping(value = "/vendor/getDefaultSetting", method = RequestMethod.GET)
  GdnRestSingleResponse<VendorDefaultFilterResponse> getDefaultSettingFilter(
      @RequestParam("vendorEmail") String vendorEmail);

  @PostMapping(value = "/api/ipr/filter/summary")
  GdnRestListResponse<IPRProductListResponse> getIPRProductList(@RequestParam("page") int page,
      @RequestParam("size") int size, @RequestBody IPRProductListRequest iprProductListRequest);

  @GetMapping(value = "/api/ipr/suspension-in-progress")
  GdnRestListResponse<IprSuspensionInProgressResponse> findSuspensionInProgressProduct(
    @RequestParam("page") int page, @RequestParam("size") int size,
    @RequestParam("businessPartnerCode") String businessPartnerCode,
    @RequestParam("sortOrder") String sortOrder);

  @GetMapping(value = "/api/ipr/details/{productSku}")
  GdnRestSingleResponse<IPRProductDetailResponse> getIPRProductDetail(
      @PathVariable("productSku") String productSku);

  @PostMapping(value = "/api/ipr/update-assignee")
  GdnBaseRestResponse updateAssignee(@RequestBody IPRUpdateAssigneeRequest iprUpdateAssigneeRequest);

  @GetMapping(value = "/api/ipr/primary-filter-counts")
  GdnRestSingleResponse<MapResponse> getPrimaryFilterCounts();

  @PostMapping(value = "/api/ipr/action")
  GdnBaseRestResponse performIprAction(@RequestBody IprActionRequest request);

  @GetMapping(value = "/api/ipr/history")
  GdnRestListResponse<IPRProductHistoryResponse> fetchIprHistory(@RequestParam("page") Integer page,
      @RequestParam("size") Integer size, @RequestParam("productSku") String productSku);

  @GetMapping(value = "/api/system-parameters/fetch-system-parameter")
  GdnRestSingleResponse<SystemParameterResponse> fetchInternalSystemParameter(
    @RequestParam("storeId") String storeId, @RequestParam("requestId") String requestId);
}
