package com.gdn.partners.pcu.internal.client.feign;

import com.gda.mta.product.dto.AutoApprovalTypeRequest;
import com.gda.mta.product.dto.DeleteProductRequest;
import com.gda.mta.product.dto.GenericStringListRequest;
import com.gda.mta.product.dto.ProductAndL5MigrationRequest;
import com.gda.mta.product.dto.ProductBusinessPartnerMapperResponse;
import com.gda.mta.product.dto.ProductCollectionResponse;
import com.gda.mta.product.dto.ProductHistoryRequest;
import com.gda.mta.product.dto.ProductHistoryResponse;
import com.gda.mta.product.dto.ProductImagePredictionAndCategoryMappingRequest;
import com.gda.mta.product.dto.ProductImagePredictionAndCategoryMappingResponse;
import com.gda.mta.product.dto.ProductImagePredictionResponse;
import com.gda.mta.product.dto.ProductImagePredictionRequest;
import com.gda.mta.product.dto.ProductRevisionInfoResponse;
import com.gda.mta.product.dto.ScreeningProductBulkActionsRequest;
import com.gda.mta.product.dto.SummaryFilterRequest;
import com.gda.mta.product.dto.SuspensionProductRequest;
import com.gda.mta.product.dto.response.AssigneeResponse;
import com.gda.mta.product.dto.response.AutoApprovalTypeResponse;
import com.gda.mta.product.dto.response.FilterCountResponse;
import com.gda.mta.product.dto.response.HalalProductHistoryResponse;
import com.gda.mta.product.dto.response.PredictionTypeResponse;
import com.gda.mta.product.dto.response.ProductSkuResponseList;
import com.gda.mta.product.dto.response.ProductSuspensionHistoryResponse;
import com.gda.mta.product.dto.response.ReviewProductResponse;
import com.gda.mta.product.dto.response.SequenceResponse;
import com.gda.mta.product.dto.response.SimpleBooleanResponse;
import com.gda.mta.product.dto.response.SuspensionProductResponse;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.mta.product.util.GdnRestSimpleResponse;
import com.gdn.mta.product.util.SingleValueResponse;
import com.gdn.partners.pbp.dto.productlevel3.ProductCollectionCountRestResponse;
import com.gdn.partners.pcu.internal.client.factory.PBPFeignFallbackFactory;
import com.gdn.partners.pcu.internal.client.model.request.SystemParameterRequest;
import com.gdn.partners.pcu.internal.client.model.response.ProductDetailCompleteResponse;
import com.gdn.partners.pcu.internal.client.model.response.ProductL3BasicResponse;
import com.gdn.partners.pcu.internal.client.model.response.ProductSystemParameterResponse;
import com.gdn.x.productcategorybase.dto.request.ProductRequest;
import com.gdn.x.productcategorybase.dto.request.solr.AttributeReqModel;
import com.gdn.x.productcategorybase.dto.response.ProductCodeResponse;
import com.gdn.x.productcategorybase.dto.response.ProductDetailResponse;

import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;

import java.util.List;

/**
 * Created by govind on 14/01/2019 AD.
 */
@FeignClient(name = "pbpFeign", url = "${service.pbp.endpoint}", fallbackFactory = PBPFeignFallbackFactory.class)
public interface PBPFeign {

  @RequestMapping(value = "/api/product/merge", method = RequestMethod.POST)
  GdnBaseRestResponse mergeProducts(@RequestParam("masterProductId") String masterProductId,
      @RequestParam("duplicateProductId") String duplicateProductId,
      @RequestParam(value = "forceMerge", defaultValue = "false", required = false) Boolean forceMerge);

  @RequestMapping(value = "/api/product/updateAndPublishToPDT", method = RequestMethod.POST)
  GdnBaseRestResponse updateAndPublishProductToPDT(@RequestBody ProductRequest request);

  @RequestMapping(value = "/api/product/generate/barcode", method = RequestMethod.GET)
  GdnRestSimpleResponse<String> generateBarcode();

  @RequestMapping(value = "/api/product/{id}", method = RequestMethod.GET)
  GdnRestSingleResponse<ProductDetailResponse> getProduct(@PathVariable("id") String id);

  @RequestMapping(value = "/api/product/update", method = RequestMethod.POST)
  GdnBaseRestResponse updateProduct(@RequestBody ProductRequest request);

  @RequestMapping(value = "/api/product-level3/updateProductAssignment", method = RequestMethod.PUT)
  GdnBaseRestResponse productAssignment(@RequestParam("productCode") String productCode,
      @RequestParam("assignedTo") String assignedTo, @RequestParam("assignedBy") String assignedBy);

  @RequestMapping(value = "/api/product/detail/filter/product-code/{productCode}", method = RequestMethod.GET)
  GdnRestSingleResponse<ProductDetailCompleteResponse> getProductDetail(@PathVariable("productCode") String productCode,
      @RequestParam("inAllProducts") boolean inAllProducts,
      @RequestParam("businessPartnerCode") String businessPartnerCode);

  @RequestMapping(value = "/api/filters/count", method = RequestMethod.GET)
  GdnRestSingleResponse<FilterCountResponse> getFilterCounts(@RequestParam("activated") boolean activated,
      @RequestParam("viewable") boolean viewable);

  @RequestMapping(value = "/api/filters/getProducts", method = RequestMethod.POST)
  GdnRestListResponse<ReviewProductResponse> getReviewProducts(@RequestBody SummaryFilterRequest request,
      @RequestParam("activated") boolean activated, @RequestParam("viewable") boolean viewable,
      @RequestParam("page") int page, @RequestParam("size") int size);

  @RequestMapping(value = "/api/filters/businessPartners", method = RequestMethod.POST)
  GdnRestListResponse<ProductBusinessPartnerMapperResponse> getBusinessPartnersByTimeAndStatusFilter(
      @RequestBody SummaryFilterRequest request, @RequestParam("activated") boolean activated,
      @RequestParam("viewable") boolean viewable, @RequestParam("page") int page, @RequestParam("size") int size);

  @RequestMapping(value = "/api/product/approve-draft", method = RequestMethod.GET)
  GdnBaseRestResponse approveDraft(@RequestParam("productCode") String productCode);

  @RequestMapping(value = "/api/product/history/{id}", method = RequestMethod.GET)
  GdnRestListResponse<ProductHistoryResponse> getProductHistory(@PathVariable("id") String productId,
      @RequestParam("page") int page, @RequestParam("size") int size);

  @RequestMapping(value = "/api/filters/getAssignees", method = RequestMethod.POST)
  GdnRestListResponse<AssigneeResponse> getAssigneesByFilterRequestAndActivatedAndViewableFlag(
      @RequestBody SummaryFilterRequest request, @RequestParam("activated") boolean activated,
      @RequestParam("viewable") boolean viewable);

  @RequestMapping(value = "api/product/screeningProductsBulkActions/{actionType}", method = RequestMethod.POST)
  GdnBaseRestResponse doScreeningProductsBulkActions(
      @PathVariable("actionType") String actionType, @RequestBody ScreeningProductBulkActionsRequest request);

  @RequestMapping(value = "/api/product/item/filter/product-name/upc-code",
      method = RequestMethod.POST)
  GdnRestListResponse<ProductCodeResponse> findByNameOrUpcCode(@RequestParam("productName") String productName,
      @RequestParam("upcCode") String upcCode, @RequestParam("finalCategoryId") String finalCategoryId,
      @RequestBody List<AttributeReqModel> attributeReqModelList, @RequestParam("page") int page,
      @RequestParam("size") int size);

  @RequestMapping(value = "/api/product/filter/keyword/{keyword}", method = RequestMethod.GET)
  GdnRestListResponse<ProductCodeResponse> filterProductsBySearchKeyword(@RequestParam("keyword") String keyword,
      @RequestParam("page") int page, @RequestParam("size") int size);

  @RequestMapping(value = "api/product-level3/doSuspensionAction", method = RequestMethod.POST)
  GdnBaseRestResponse doSuspensionAction(@RequestBody SuspensionProductRequest request);

  @RequestMapping(value = "api/product/{productCode}/revisionHistory", method = RequestMethod.GET)
  GdnRestListResponse<ProductRevisionInfoResponse> getProductRevisionHistory(
      @PathVariable("productCode") String productCode);

  @RequestMapping(value = "api/product/submit-history", method = RequestMethod.POST)
  GdnBaseRestResponse submitHistory(@RequestBody ProductHistoryRequest productHistoryRequest);

  @RequestMapping(value = "api/product-level3/counts/product-by-brand", method = RequestMethod.GET)
  GdnRestSimpleResponse<Long> getProductsCountByBrandName(@RequestParam("brand") String brandName);

  @RequestMapping(value = "api/product/{productCode}/screeningNotes", method = RequestMethod.GET)
  GdnRestSingleResponse<SingleValueResponse> getProductScreeningNotes(@PathVariable("productCode") String productCode);

  @RequestMapping(value = "/api/product-level3/getAllProducts", method = RequestMethod.POST)
  GdnRestListResponse<SuspensionProductResponse> getAllProducts(@RequestBody SummaryFilterRequest request,
      @RequestParam("page") int page, @RequestParam("size") int size);

  @RequestMapping(value = "/api/product-level3/suspensionHistory", method = RequestMethod.GET)
  GdnRestListResponse<ProductSuspensionHistoryResponse> getSuspensionHistory(@RequestParam("productSku") String productSku,
      @RequestParam("page") int page, @RequestParam("size") int size);

  @RequestMapping(value = "/api/product/{productCode}/update-product-category", method = RequestMethod.PUT)
  GdnBaseRestResponse updateProductCategory(@PathVariable("productCode") String productCode,
      @RequestParam("categoryCode") String categoryCode);

  @RequestMapping(value = "/api/product/collection/filter/keyword", method = RequestMethod.GET)
  GdnRestListResponse<ProductCollectionResponse> filterProductCollectionSummaryByKeyword(
      @RequestParam("businessPartnerCode") String businessPartnerCode,
      @RequestParam("categoryCode") String categoryCode, @RequestParam("keyword") String keyword,
      @RequestParam("reviewPending") Boolean reviewPending, @RequestParam("activated") boolean activated,
      @RequestParam("viewable") boolean viewable, @RequestParam("sortBy") String sortBy, @RequestParam("page") int page,
      @RequestParam("size") int size);

  @RequestMapping(value = "/api/product/count", method = RequestMethod.GET)
  GdnRestSingleResponse<ProductCollectionCountRestResponse> countProductCollectionBySpecifiedDateRange(
      @RequestParam("businessPartnerCode") String businessPartnerCode,
      @RequestParam("categoryCode") String categoryCode, @RequestParam("keyword") String keyword,
      @RequestParam("activated") boolean activated, @RequestParam("viewable") boolean viewable);

  @RequestMapping(value = "/api/productImagePrediction/getDifferentPredictionType",
      method = RequestMethod.GET)
  GdnRestListResponse<PredictionTypeResponse> getDifferentPredictionType();

  @RequestMapping(value = "/api/productImagePrediction/prediction-list", method = RequestMethod.GET)
  GdnRestListResponse<ProductImagePredictionResponse> getListOfPredictions();

  @RequestMapping(value = "/api/product/history/{id}", method = RequestMethod.GET)
  GdnRestListResponse<ProductHistoryResponse> getProductHistorySummary(@RequestParam("page") int page,
      @RequestParam("size") int size, @PathVariable("id") String id);

  @RequestMapping(value = "/api/product/collection/filter/keyword/age-between", method = RequestMethod.GET)
  GdnRestListResponse<ProductCollectionResponse> filterProductCollectionSummaryByKeywordAndAgeBetween(
      @RequestParam("page") Integer page, @RequestParam("size") Integer size,
      @RequestParam("businessPartnerCode") String businessPartnerCode,
      @RequestParam("categoryCode") String categoryCode, @RequestParam("keyword") String keyword,
      @RequestParam("startAge") String startAge, @RequestParam("endAge") String endAge,
      @RequestParam("timeFilterType") String timeFilterType, @RequestParam("activated") boolean activated,
      @RequestParam("viewable") boolean viewable);

  @RequestMapping(value = "/api/product/collection/filter/keyword/age-less-than", method = RequestMethod.GET)
  GdnRestListResponse<ProductCollectionResponse> filterProductCollectionSummaryByKeywordAndAgeLessThan(
      @RequestParam("page") Integer page, @RequestParam("size") Integer size,
      @RequestParam("businessPartnerCode") String businessPartnerCode,
      @RequestParam("categoryCode") String categoryCode, @RequestParam("keyword") String keyword,
      @RequestParam("age") String age, @RequestParam("timeFilterType") String timeFilterType,
      @RequestParam("activated") boolean activated, @RequestParam("viewable") boolean viewable);

  @RequestMapping(value = "/api/product/publishProductToPDT", method = RequestMethod.GET)
  GdnBaseRestResponse publishProductToPDT(@RequestParam("productCode") String productCode);

  @RequestMapping(value = "/api/product-level3/compareProductAndCategoryWholesale", method = RequestMethod.GET)
  GdnBaseRestResponse compareProductAndCategoryWholesale(@RequestParam("productCode") String productCode,
      @RequestParam("categoryCode") String categoryCode);

  @RequestMapping(value = "/api/sequence/find-sequence-by-key", method = RequestMethod.GET)
  GdnRestSingleResponse<SequenceResponse> findCounterByKey(@RequestParam("key") String key);

  @RequestMapping(value = "/api/solr/reindex/prd-collection/reindexInReviewProductByProductCode", method = RequestMethod.POST)
  GdnBaseRestResponse reindexByProductCode(@RequestParam("productCode") String productCode);

  @RequestMapping(value = "/api/solr/reindex/prd-collection/reindexActiveProductByProductCode", method = RequestMethod.POST)
  GdnBaseRestResponse reindexActiveProductByProductCode(@RequestParam("productCode") String productCode);

  @RequestMapping(value = "/api/productImagePrediction/update", method = RequestMethod.PUT)
  GdnBaseRestResponse updateImagePrediction(@RequestBody ProductImagePredictionRequest productImagePredictionRequest);

  @RequestMapping(value = "/api/productImagePrediction/updatePredictionAndCategoryMapping", method = RequestMethod.PUT)
  GdnBaseRestResponse updateImagePredictionAndCategoryMapping(
      @RequestBody ProductImagePredictionAndCategoryMappingRequest productImagePredictionAndCategoryMappingRequest);

  @RequestMapping(value = "/api/productImagePrediction/getImagePredictionAndCategoryMapping", method = RequestMethod.POST)
  GdnRestListResponse<ProductImagePredictionAndCategoryMappingResponse> getImagePredictionAndCategoryMapping(
      @RequestBody GenericStringListRequest predictionTypeList);

  @RequestMapping(value = "/api/product/{productCode}/publish-edited-event", method = RequestMethod.GET)
  GdnBaseRestResponse publishEditedEvent(@PathVariable("productCode") String productCode,
      @RequestParam("reviewType") String reviewType);

  @RequestMapping(value = "/api/product/{productCode}/publish-revised-event", method = RequestMethod.GET)
  GdnBaseRestResponse publishRevisedEvent(@PathVariable("productCode") String productCode);

  @RequestMapping(value = "/api/product/{productCode}/retry-resize-edited-image", method = RequestMethod.PUT)
  GdnBaseRestResponse retryEditedResizeImage(@PathVariable("productCode") String productCode);

  @RequestMapping(value = "/api/product/{productCode}/update-pbp-product-workflow", method = RequestMethod.GET)
  GdnBaseRestResponse updateProductWorkflow(@PathVariable("productCode") String productCode,
      @RequestParam("state") String state);

  @RequestMapping(value = "/api/product/{productCode}/update-review-pending", method = RequestMethod.PUT)
  GdnBaseRestResponse updateReviewPending(@PathVariable("productCode") String productCode,
      @RequestParam("reviewPending") boolean reviewPending);

  @RequestMapping(value = "/api/product/{productCode}/update-activated-and-viewable", method = RequestMethod.PUT)
  GdnBaseRestResponse updateActivatedAndViewable(@PathVariable("productCode") String productCode,
      @RequestParam("activated") boolean activated, @RequestParam("viewable") boolean viewable);

  @RequestMapping(value = "/api/product/resizeImage", method = RequestMethod.PUT)
  GdnBaseRestResponse retryResizeImage(@RequestParam("productCode") String productCode,
      @RequestParam("deltaIndex") boolean deltaReindex);

  @RequestMapping(value = "/api/product/{productCode}/check-auto-approval-eligibility", method = RequestMethod.POST)
  GdnRestSingleResponse<AutoApprovalTypeResponse> checkAutoApprovalEligibility(
      @PathVariable("productCode") String productCode, @RequestBody AutoApprovalTypeRequest autoApprovalTypeRequest);

  @RequestMapping(value = "api/product/delete-product-collection", method = RequestMethod.POST)
  GdnBaseRestResponse deleteProductCollection(@RequestParam("storeId") String storeId,
    @RequestParam("channelId") String channelId, @RequestParam("clientId") String clientId,
    @RequestParam("requestId") String requestId, @RequestParam("username") String username,
    @RequestParam("needEmailNotification") boolean needEmailNotification,
    @RequestBody DeleteProductRequest deleteProductRequest);

  @RequestMapping(value = "/api/product/{productSku}/get-halal-product-history-by-productSku", method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
  GdnRestListResponse<HalalProductHistoryResponse> getHalalProductHistory(@PathVariable("productSku") String productSku,
      @RequestParam("page") Integer page, @RequestParam("size") Integer size);

  @RequestMapping(value = "/api/product/getProductSkusByProductCode", method = RequestMethod.GET)
  GdnRestSimpleResponse<ProductSkuResponseList> getProductSkuByProductCode(
      @RequestParam("productCode") String productCode);

  @PutMapping(value = "/api/productSystemParameter/update", produces = MediaType.APPLICATION_JSON_VALUE)
  GdnBaseRestResponse updateSystemParameter(@RequestParam("storeId") String storeId,
      @RequestParam("channelId") String channelId, @RequestParam("clientId") String clientId,
      @RequestParam("requestId") String requestId, @RequestParam("username") String username,
      @RequestBody SystemParameterRequest productSystemParameterRequest);

  @GetMapping(value = "/api/productSystemParameter/fetchSwitchValuesWithShowOnUI", produces =
      MediaType.APPLICATION_JSON_VALUE)
  GdnRestListResponse<ProductSystemParameterResponse> fetchSystemParameterShowOnUI(
      @RequestParam("storeId") String storeId, @RequestParam("channelId") String channelId,
      @RequestParam("clientId") String clientId, @RequestParam("requestId") String requestId);

  @RequestMapping(value = "/api/product/migrateProductAndL5DetailsByProductSku", method = RequestMethod.POST)
  GdnBaseRestResponse migrateProductAndL5DetailsByProductSku(
    @RequestParam("storeId") String storeId, @RequestParam("channelId") String channelId,
    @RequestParam("clientId") String clientId, @RequestParam("requestId") String requestId,
    @RequestParam("username") String username,
    @RequestBody ProductAndL5MigrationRequest productAndL5MigrationRequest);


  @GetMapping(value = "/api/product-level3/v2/{productCode}/basicDetails", produces =
      MediaType.APPLICATION_JSON_VALUE)
  GdnRestSingleResponse<ProductL3BasicResponse> getProductLevel3BasicDetails(
      @PathVariable("productCode") String productCode);
}
