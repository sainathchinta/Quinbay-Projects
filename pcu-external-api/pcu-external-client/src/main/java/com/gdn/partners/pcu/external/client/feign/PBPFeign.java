package com.gdn.partners.pcu.external.client.feign;

import com.gda.mta.product.dto.AppealProductRequest;
import com.gda.mta.product.dto.AppealProductResponse;
import com.gda.mta.product.dto.BulkDeleteProductWipRequest;
import com.gda.mta.product.dto.BulkDownloadProductLevel3Response;
import com.gda.mta.product.dto.EditProductV2Response;
import com.gda.mta.product.dto.HistoryRequest;
import com.gda.mta.product.dto.HistoryUpdateRequest;
import com.gda.mta.product.dto.ItemPickupPointListingL3Request;
import com.gda.mta.product.dto.LogAuditTrailUpdatedProductResponse;
import com.gda.mta.product.dto.NeedRevisionSubmitRequest;
import com.gda.mta.product.dto.OmniChannelExistsRequest;
import com.gda.mta.product.dto.PickupPointUpdateRequest;
import com.gda.mta.product.dto.PickupPointUpdateResponse;
import com.gda.mta.product.dto.ProductBusinessPartnerRequest;
import com.gda.mta.product.dto.ProductCopyRequest;
import com.gda.mta.product.dto.response.OmniChannelMapAndSkuResponse;
import com.gdn.partners.pcu.external.client.model.ProductCreationRequest;
import com.gda.mta.product.dto.ProductImageEditRequest;
import com.gda.mta.product.dto.ProductLevel3OrderResponse;
import com.gda.mta.product.dto.ProductLevel3QuickEditRequest;
import com.gda.mta.product.dto.ProductLevel3QuickEditV2Request;
import com.gda.mta.product.dto.ProductLevel3Request;
import com.gda.mta.product.dto.ProductLevel3Response;
import com.gda.mta.product.dto.ProductLevel3StockInfoWebSiteResponse;
import com.gda.mta.product.dto.ProductLevel3StockRequest;
import com.gda.mta.product.dto.ProductLevel3SummaryCountResponse;
import com.gda.mta.product.dto.ProductLevel3SummaryDetailsRequest;
import com.gda.mta.product.dto.ProductLevel3SummaryDetailsResponse;
import com.gda.mta.product.dto.ProductLevel3SummaryRequest;
import com.gda.mta.product.dto.ProductLevel3SummaryResponse;
import com.gda.mta.product.dto.ProductLevel3UpdateRequest;
import com.gda.mta.product.dto.ProductLevel3UpdateSummaryRequest;
import com.gda.mta.product.dto.ProductPriceAndWholesaleRequest;
import com.gda.mta.product.dto.ProductSkuAndPickupPointCodeRequest;
import com.gda.mta.product.dto.ProductSkuListRequest;
import com.gda.mta.product.dto.ProductSystemParameterResponse;
import com.gda.mta.product.dto.ProductVariantUpdateRequest;
import com.gda.mta.product.dto.RejectedSkuProductResponse;
import com.gda.mta.product.dto.SummaryFilterRequest;
import com.gda.mta.product.dto.UpdateImageRequest;
import com.gda.mta.product.dto.UpdateItemsPriceStockImagesRequest;
import com.gda.mta.product.dto.UpdateProductLevel3InfoRequest;
import com.gda.mta.product.dto.VendorNotesRequest;
import com.gda.mta.product.dto.response.AvailableToCopyProductDetailsResponse;
import com.gda.mta.product.dto.response.CogsValueResponse;
import com.gda.mta.product.dto.response.HistoryResponse;
import com.gda.mta.product.dto.response.HistoryUpdateResponse;
import com.gda.mta.product.dto.response.ItemBulkArchiveResponse;
import com.gda.mta.product.dto.response.ItemPickupPointListingL3Response;
import com.gda.mta.product.dto.response.ItemSummaryL4Response;
import com.gda.mta.product.dto.response.ItemsPriceStockImagesUpdateResponse;
import com.gda.mta.product.dto.response.ProductCountResponse;
import com.gda.mta.product.dto.response.ProductItemNameResponse;
import com.gda.mta.product.dto.response.ProductLevel3DetailResponse;
import com.gda.mta.product.dto.response.ProductLevel3DetailsV2Response;
import com.gda.mta.product.dto.response.ProductSkuResponseList;
import com.gda.mta.product.dto.response.ProductSuspensionHistoryResponse;
import com.gda.mta.product.dto.response.ProductSystemParameterSwitchResponse;
import com.gda.mta.product.dto.response.VendorNotesResponse;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.mta.product.entity.PickupPointCodeResponse;
import com.gdn.mta.product.entity.UniquePickupPointCodeResponse;
import com.gdn.mta.product.util.GdnRestSimpleResponse;
import com.gdn.partners.pbp.dto.productcategory.CategoryHierarchyProductCountResponse;
import com.gdn.partners.pbp.dto.productlevel1.ProductSearchRequest;
import com.gdn.partners.pbp.dto.productlevel3.CountProductLevel3InactiveResponse;
import com.gdn.partners.pbp.dto.productlevel3.CountProductLevel3WipResponse;
import com.gdn.partners.pbp.dto.productlevel3.EstimateItemPriceResponse;
import com.gdn.partners.pbp.dto.productlevel3.PostLiveProductCountResponse;
import com.gdn.partners.pbp.dto.productlevel3.ProductLevel3CountResponse;
import com.gdn.partners.pbp.dto.productlevel3.ProductLevel3WipDetailResponse;
import com.gdn.partners.pbp.dto.productlevel3.ProductLevel3WipResponse;
import com.gdn.partners.pbp.dto.productlevel3.ProductLevel3WipSummaryRequest;
import com.gdn.partners.pbp.dto.productlevel3.SuspensionItemResponse;
import com.gdn.partners.pcu.external.client.factory.PBPFeignFallbackFactory;
import com.gdn.partners.pcu.external.web.model.request.DistributionInfoUpdateRequest;
import com.gdn.partners.pcu.external.web.model.response.AppealProductConfigResponse;
import com.gdn.partners.pcu.external.web.model.response.ProductL3DetailsResponse;
import com.gdn.x.product.rest.web.model.request.SimpleListStringRequest;
import com.gdn.x.productcategorybase.dto.request.OmniChannelSkuRequest;
import com.gdn.x.productcategorybase.dto.response.PredefinedAllowedAttributeValueResponse;
import com.gdn.x.productcategorybase.dto.response.ProductItemDetailResponse;
import com.gdn.x.productcategorybase.dto.response.ProductItemResponse;
import com.gdn.x.productcategorybase.dto.response.ValidOmniChannelSkuResponse;
import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;

@FeignClient(name = "pbpFeign", url = "${service.pbp.endpoint}", fallbackFactory = PBPFeignFallbackFactory.class)
public interface PBPFeign {

  @RequestMapping(value = "/api/product-business-partner/create", method = RequestMethod.POST,
                  produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  GdnBaseRestResponse create(@RequestBody ProductBusinessPartnerRequest request);

  @RequestMapping(value = "/api/product/create-product", method = RequestMethod.POST,
                  produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  GdnBaseRestResponse createProduct(@RequestBody ProductCreationRequest request, @RequestParam("flowType") String flowType);

  @RequestMapping(value = "/api/product/create-new-product", method = RequestMethod.POST,
      produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  GdnBaseRestResponse createNewProduct(@RequestBody ProductCreationRequest request, @RequestParam("flowType") String flowType);

  @RequestMapping(value = "/api/product/generate-product-code", method = RequestMethod.GET,
      produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  GdnRestSimpleResponse<String> generateProductCode();

  @RequestMapping(value = "/api/product-level3/get-estimate-price-for-item",
      method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
  GdnRestSimpleResponse<EstimateItemPriceResponse> getEstimatedPrice(@RequestParam("itemCode") String itemCode,
      @RequestParam("lowestPriceCoefficient") double lowestPriceCoefficient);

  @RequestMapping(value = "/api/product/generate/barcode", method = RequestMethod.GET,
      produces = MediaType.APPLICATION_JSON_VALUE) GdnRestSimpleResponse<String> generateBarCode();

  @RequestMapping(value = "/api/product/item-detail/filter/product-name-and-category-codes", method = RequestMethod.POST, produces =
      MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  GdnRestListResponse<ProductItemDetailResponse> getProductItemsByNameAndCategoryCodes(
      @RequestParam("page") Integer page, @RequestParam("size") Integer size, @RequestBody ProductSearchRequest request,
      @RequestParam("isOnlyExternal") boolean isOnlyExternal);

  @RequestMapping(value = "/api/product/item/filter/name-and-category-id", method = RequestMethod.GET, produces =
      MediaType.APPLICATION_JSON_VALUE)
  GdnRestListResponse<ProductItemResponse> getProductItemSuggestions(@RequestParam("page") Integer page,
      @RequestParam("size") Integer size, @RequestParam("productItemName") String productItemName,
      @RequestParam("categoryId") String categoryId);

  @GetMapping(value = "/api/product/filter/category-hierarchy-with-product-count/keyword", produces = MediaType.APPLICATION_JSON_VALUE)
  GdnRestListResponse<CategoryHierarchyProductCountResponse> getCategoryHierarchyByKeywordWithProductCount(
    @RequestParam("page") Integer page, @RequestParam("size") Integer size,
    @RequestParam("keyword") String keyword,
    @RequestParam("businessPartnerCode") String businessPartnerCode);

  @GetMapping(value = "/api/product/count/viewable/{viewable}", produces = MediaType.APPLICATION_JSON_VALUE)
  GdnRestSimpleResponse<Integer> getProductsCountByViewable(@PathVariable("viewable") boolean viewable);

  @GetMapping(value = "/api/product-level3/wip/detail/filter/product-sku", produces = MediaType.APPLICATION_JSON_VALUE)
  GdnRestSingleResponse<ProductLevel3WipDetailResponse> getProductDetailByProductSku(
      @RequestParam("productSku") String productSku, @RequestParam("isActive") boolean isActive);

  @GetMapping(value = "/api/product-level3/filter/detail/all-product", produces = MediaType.APPLICATION_JSON_VALUE)
  GdnRestSingleResponse<ProductLevel3Response> findDetailByGdnSku(
      @RequestParam("businessPartnerCode") String businessPartnerCode, @RequestParam("gdnSku") String gdnSku);

  @RequestMapping(value = "/api/product-business-partner/products/copy", method = RequestMethod.POST,
    produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  GdnBaseRestResponse copy(@RequestParam("isRetryAttempt") boolean isRetryAttempt, @RequestBody ProductCopyRequest request);

  @RequestMapping(value = "/api/product-business-partner/products/copy/all", method = RequestMethod.POST,
    produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  GdnBaseRestResponse copyAll(@RequestBody ProductCopyRequest request);

  @RequestMapping(value = "/api/product/qrcode/notification", method = RequestMethod.PUT, produces =
      MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  GdnBaseRestResponse generateQRCodeNotification(@RequestParam("businessPartnerCode") String businessPartnerCode,
      @RequestParam("filePath") String filePath);

  @RequestMapping(value = "/api/product-level3/getSuspendedItems", method = RequestMethod.POST,
      produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  GdnRestListResponse<SuspensionItemResponse> getSuspendedItem(@RequestParam("page") Integer page,
      @RequestParam("size") Integer size, @RequestBody SummaryFilterRequest request);

  @RequestMapping(value = "/api/product-level3/filter/products/copy", method = RequestMethod.POST,
    produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  GdnRestListResponse<AvailableToCopyProductDetailsResponse> productsAvailableToCopy(
    @RequestParam("businessPartnerCode") String businessPartnerCode,
    @RequestParam("linkedPartnerCode") String linkedPartnerCode, @RequestParam("page") Integer page,
    @RequestParam("size") Integer size, @RequestBody ProductLevel3SummaryRequest request);

  @GetMapping(value = "/api/product-level3/counts/summary/{businessPartnerCode}", produces = {MediaType.APPLICATION_JSON_VALUE})
  GdnRestSingleResponse<ProductLevel3SummaryCountResponse> getActiveProductStockCount(
      @PathVariable("businessPartnerCode") String businessPartnerCode);

  @GetMapping(value = "/api/product-level3/wip/count/summary-with-state", produces = {MediaType.APPLICATION_JSON_VALUE})
  GdnRestSingleResponse<CountProductLevel3WipResponse> getInProgressProductCount(
      @RequestParam("businessPartnerCode") String businessPartnerCode);

  @GetMapping(value = "/api/product-level3/wip/count/summary-by-filter-type", produces = {
      MediaType.APPLICATION_JSON_VALUE})
  GdnRestSingleResponse<ProductLevel3CountResponse> getNonActiveProductCount(
      @RequestParam("businessPartnerCode") String businessPartnerCode, @RequestParam("type") String type);

  @GetMapping(value = "/api/product-level3/count/inactive-products", produces = {MediaType.APPLICATION_JSON_VALUE})
  GdnRestSingleResponse<CountProductLevel3InactiveResponse> getInActiveProductCount(
      @RequestParam("businessPartnerCode") String businessPartnerCode);

  @RequestMapping(value = "/api/product-level3/filter/summary", method = RequestMethod.POST,
      produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  GdnRestListResponse<ProductLevel3SummaryResponse> filterSummary(
      @RequestParam("businessPartnerCode") String businessPartnerCode, @RequestParam("page") Integer page,
      @RequestParam("size") Integer size, @RequestParam("orderBy") String orderBy, @RequestParam("sortBy") String sortBy,
      @RequestBody ProductLevel3SummaryRequest request);

  @RequestMapping(value = "/api/product-business-partner/filter/rejected-sku", method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
  GdnRestListResponse<RejectedSkuProductResponse> filterProductBusinessPartnerSummaryByBusinessPartnerId(
      @RequestParam("page") Integer page, @RequestParam("size") Integer size,
      @RequestParam("businessPartnerId") String businessPartnerId,
      @RequestParam("searchCriteria") String searchCriteria, @RequestParam("orderBy") String orderBy, @RequestParam("sortBy") String sortBy);

  @RequestMapping(value = "/api/product-level3/wip/filter/summary-with-state", method = RequestMethod.POST, produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  GdnRestListResponse<ProductLevel3WipResponse> filterSummaryWithState(@RequestParam("page") Integer page,
      @RequestParam("size") Integer size, @RequestBody ProductLevel3WipSummaryRequest request);

  @RequestMapping(value = "/api/product-level3/item/archive", method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
  GdnBaseRestResponse toggleArchiveItem(@RequestParam("clientHost") String clientHost, @RequestParam("itemSku") String itemSku,
      @RequestParam("doArchive") boolean doArchive);

  @RequestMapping(value = "/api/audit-trial/product-update-logs", method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
  GdnRestListResponse<LogAuditTrailUpdatedProductResponse> getProductUpdateLogs(@RequestParam("page") Integer page,
      @RequestParam("size") Integer size, @RequestParam("gdnSku") String gdnSku);

  @RequestMapping(value = "/api/product-level3/filter/detail/order", method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
  GdnRestSingleResponse<ProductLevel3OrderResponse> filterDetailOrderByGdnSku(
      @RequestParam("businessPartnerCode") String businessPartnerCode, @RequestParam("gdnSku") String gdnSku);

  @RequestMapping(value = "/api/product-level3/product/update-summary", method = RequestMethod.POST,
      produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  GdnRestSingleResponse<ProductLevel3SummaryResponse> updateSummary(@RequestParam("businessPartnerCode") String businessPartnerCode, @RequestParam("gdnSku") String gdnSku,
      @RequestParam("clientHost") String clientHost, @RequestBody ProductLevel3UpdateSummaryRequest request);

  @RequestMapping(value = "/api/product/bulk/product-wip/delete", method = RequestMethod.POST, produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  GdnRestSingleResponse<PostLiveProductCountResponse> bulkDeleteProductWip(@RequestParam("businessPartnerCode")
      String businessPartnerCode, @RequestBody BulkDeleteProductWipRequest request);

  @RequestMapping(value = "/api/product-business-partner/retry-create", method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
  GdnBaseRestResponse retryCreate(@RequestParam("productBusinessPartnerId") String productBusinessPartnerId);

  @RequestMapping(value = "/api/business-partner-config/enable-product-mail-notify", method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
  GdnRestSimpleResponse<Boolean> notifyMailVisibilityOptionForProductWip(
      @RequestParam("businessPartnerCode") String businessPartnerCode);

  @RequestMapping(value = "/api/product-level3/filter/summary/single", method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
  GdnRestSingleResponse<ProductLevel3SummaryResponse> filterSummaryByGdnSku(
      @RequestParam("businessPartnerCode") String businessPartnerCode, @RequestParam("gdnSku") String gdnSku);

  @RequestMapping(value = "/api/product-level3/product/unsynchronize", method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
  GdnBaseRestResponse unsynchronizeProduct(@RequestParam("clientHost") String clientHost,
      @RequestParam("productSku") String productSku, @RequestParam("itemSku") String itemSku);

  @RequestMapping(value = "/api/product-level3/product/synchronize", method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
  GdnBaseRestResponse synchronizeProduct(@RequestParam("clientHost") String clientHost,
      @RequestParam("productSku") String productSku, @RequestParam("itemSku") String itemSku);

  @RequestMapping(value = "/api/product-level3/filter/activeBrandByCategoryId", method = RequestMethod.GET, consumes = MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
  GdnRestListResponse<PredefinedAllowedAttributeValueResponse> activeBrandByCategoryId(
      @RequestParam("categoryId") String categoryId);

  @RequestMapping(value = "/api/product-level3/wip/send-mail-for-exceeded-activation", method = RequestMethod.POST, produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  GdnBaseRestResponse sendEmailForExceededActivation(@RequestParam("businessPartnerCode") String businessPartnerCode);

  @RequestMapping(value = "/api/product-level3/stock-info/website", method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
  GdnRestSingleResponse<ProductLevel3StockInfoWebSiteResponse> getStockInfoWebSite(
      @RequestParam("webMerchantCode") String webMerchantCode, @RequestParam("webItemSku") String webItemSku);

  @RequestMapping(value = "/api/product/cogs-value/{materialCode}", method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
  GdnRestSimpleResponse<CogsValueResponse> getCogsValue(@PathVariable("materialCode") String materialCode);

  @RequestMapping(value = "/api/product-level3/product/update-return", method = RequestMethod.POST, consumes = MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
  GdnRestSingleResponse<ProductLevel3Response> updateAndReturn(@RequestParam("clientHost") String clientHost,
      @RequestParam("isOnlyExternal") Boolean isOnlyExternal, @RequestParam("hasOrder") boolean hasOrder,
      @RequestParam("updateLogistics") boolean updateLogistics, @RequestBody ProductLevel3Request product);

  @GetMapping(value = "/api/product-level3/pristineCategory/{categoryId}", produces = MediaType.APPLICATION_JSON_VALUE)
  GdnRestSimpleResponse<Boolean> isPristineCategory(@PathVariable("categoryId") String categoryId);

  @RequestMapping(value = "/api/product-level3/image", method = RequestMethod.POST, produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  GdnBaseRestResponse updateProductImage(@RequestParam("isOnlyExternal") boolean isOnlyExternal,
      @RequestBody UpdateImageRequest request);

  @RequestMapping(value = "/api/product-business-partner/isProductMapped/{merchantCode}", method = RequestMethod.GET,
                  produces = MediaType.APPLICATION_JSON_VALUE)
  GdnRestSimpleResponse<Boolean> isProductMappedToMerchant(@PathVariable("merchantCode") String merchantCode);

  @RequestMapping(value = "/api/product-level3/item/update-item-stock", method = RequestMethod.POST, produces =
      MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  GdnBaseRestResponse updateItemStock(@RequestParam("clientHost") String clientHost,
      @RequestParam("businessPartnerCode") String businessPartnerCode,
      @RequestBody ProductLevel3StockRequest productLevel3StockRequest);

  @RequestMapping(value = "/api/product-level3/item/update-item-price", method = RequestMethod.POST, produces =
      MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  GdnBaseRestResponse updateItemPrice(@RequestParam("clientHost") String clientHost,
      @RequestParam("itemSku") String itemSku,
      @RequestBody ProductPriceAndWholesaleRequest productPriceAndWholesaleRequest);

  @RequestMapping(value = "/api/product/minimum-price", method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
  GdnRestSimpleResponse<Integer> getMinimumPrice();

  @RequestMapping(value = "/api/productSystemParameter/find", method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
  GdnRestSingleResponse<ProductSystemParameterResponse> findSystemParameter(@RequestParam("variable") String variable);

  @RequestMapping(value = "/api/productSystemParameter/fetchSwitchValues", method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
  GdnRestSingleResponse<ProductSystemParameterSwitchResponse> getSystemParameterSwitch();

  @RequestMapping(value = "/api/product-level3/product/{productSku}/edit-info", method = RequestMethod.POST, consumes = MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
  GdnRestSingleResponse<EditProductV2Response> editProductInfo(@RequestBody ProductLevel3Request product,
      @PathVariable("productSku") String productSku, @RequestParam("isOnlyExternal") boolean isOnlyExternal);

  @RequestMapping(value = "/api/product-level3/filter/summary/details", method = RequestMethod.POST, produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  GdnRestListResponse<ProductLevel3SummaryDetailsResponse> filterSummaryDetails(
      @RequestParam("businessPartnerCode") String businessPartnerCode, @RequestParam("page") Integer page,
      @RequestParam("size") Integer size, @RequestBody ProductLevel3SummaryDetailsRequest request);

  @RequestMapping(value = "/api/product-level3/getPickupPointCodes", method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
  GdnRestListResponse<PickupPointCodeResponse> getPickupPointCodes(@RequestParam("page") int page,
      @RequestParam("size") int size, @RequestParam("productSku") String productSku,
      @RequestParam("needCorrection") boolean needCorrection,
      @RequestParam("businessPartnerCode") String businessPartnerCode,
      @RequestParam("fbbActivated") boolean fbbActivated);

  @RequestMapping(value = "/api/product-level3/getUniquePickupPointCodes", method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
  GdnRestSingleResponse<UniquePickupPointCodeResponse> getUniquePickupPointCodes(
      @RequestParam("productSku") String productSku);

  @RequestMapping(value = "/api/product-level3/product/update-items-price-stock", method = RequestMethod.POST, produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  GdnRestSingleResponse<ItemsPriceStockImagesUpdateResponse> updateItemsPriceStockImages(
      @RequestParam("businessPartnerCode") String businessPartnerCode,
      @RequestBody UpdateItemsPriceStockImagesRequest updateItemsPriceStockImagesRequest);

  @RequestMapping(value = "/api/filters/getProductEditHistory", method = RequestMethod.POST, produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  GdnRestListResponse<HistoryResponse> getProductHistorySummary(@RequestParam("page") int page,
      @RequestParam("size") int size, @RequestBody HistoryRequest historyRequest);

  @RequestMapping(value = "/api/product-level3/product/update-logistics", method = RequestMethod.POST, consumes = MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
  GdnBaseRestResponse updateLogistics(@RequestParam("isOnlyExternal") Boolean isOnlyExternal,
      @RequestBody ProductLevel3UpdateRequest product, @RequestParam("isNeedCorrection") boolean isNeedCorrection);

  @RequestMapping(value = "/api/product/getProductSkusByProductCode", method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
  GdnRestSimpleResponse<ProductSkuResponseList> getProductSkusByProductCode(@RequestParam("productCode") String productCode);

  @RequestMapping(value = "/api/product-level3/updatePickupPointCodes", method = RequestMethod.POST, consumes = MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
  GdnRestSingleResponse<PickupPointUpdateResponse>  updatePickupPointCodes(@RequestBody PickupPointUpdateRequest pickupPointUpdateRequest);

  @RequestMapping(value = "/api/product-level3/getProductVariantsName", method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
  GdnRestListResponse<ProductItemNameResponse> getProductVariantsName(@RequestParam("page") int page,
      @RequestParam("size") int size, @RequestParam("productSku") String productSku);

  @RequestMapping(value = "/api/product-level3/getDetailByProductSku", method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
  GdnRestSingleResponse<ProductLevel3DetailResponse> getL3DetailByProductSku(
      @RequestParam("productSku") String productSku, @RequestParam("isNeedCorrection") boolean isNeedCorrection);

  @RequestMapping(value = "/api/product-level3/{productSku}/item-listing-update", method = RequestMethod.POST, produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  GdnBaseRestResponse itemListingUpdate(@RequestBody ProductLevel3QuickEditRequest request,  @PathVariable("productSku") String productSku);

  @RequestMapping(value = "/api/product-level3/archive", method = RequestMethod.POST, consumes = MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
  GdnRestSingleResponse<ItemBulkArchiveResponse> archiveProducts(@RequestParam("doArchive") boolean doArchive,
      @RequestBody SimpleListStringRequest request);

  @RequestMapping(value = "/api/product-level3/getSuspensionNotesByProductSkus", method = RequestMethod.POST, consumes = MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
  GdnRestListResponse<ProductSuspensionHistoryResponse> fetchProductSuspensionHistory(
      @RequestBody ProductSkuListRequest productSkuListRequest);

  @RequestMapping(value = "/api/product/{productCode}/vendorNotes", method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
  GdnRestSimpleResponse<VendorNotesResponse> getVendorNotes(@PathVariable("productCode") String productCode);

  @RequestMapping(value = "/api/product/{productCode}/updateVendorNotes", method = RequestMethod.PUT, produces = {MediaType.APPLICATION_JSON_VALUE}, consumes = {MediaType.APPLICATION_JSON_VALUE})
  GdnBaseRestResponse updateVendorNotes(@PathVariable("productCode") String productCode, @RequestBody VendorNotesRequest request);


  @RequestMapping(value = "/api/product/need-revision-submit", method = RequestMethod.POST, produces = {
      MediaType.APPLICATION_JSON_VALUE}, consumes = {MediaType.APPLICATION_JSON_VALUE})
  GdnRestSingleResponse<EditProductV2Response> submitNeedRevisionProduct(@RequestBody NeedRevisionSubmitRequest request);

  @RequestMapping(value = "/api/product/appeal-product", method = RequestMethod.POST, produces = {
      MediaType.APPLICATION_JSON_VALUE}, consumes = {MediaType.APPLICATION_JSON_VALUE})
  GdnRestSingleResponse<AppealProductResponse> updateAppealInProgressProduct(
      @RequestBody AppealProductRequest appealProductRequest);

  @RequestMapping(value = "/api/product-level3/{productSku}/update-product-level3-info", method = RequestMethod.POST, produces = {
      MediaType.APPLICATION_JSON_VALUE}, consumes = {MediaType.APPLICATION_JSON_VALUE})
  GdnRestSingleResponse<EditProductV2Response> updateProductInfo(@RequestBody UpdateProductLevel3InfoRequest request,
      @PathVariable("productSku") String productSku);

  @RequestMapping(value = "/api/product-level3/updateImages", method = RequestMethod.POST, produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  GdnRestSingleResponse<ItemsPriceStockImagesUpdateResponse> updateImages(@RequestBody ProductImageEditRequest productImageEditRequest);

  @RequestMapping(value = "/api/filters/getProductUpdateHistory", method = RequestMethod.POST,
    produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  GdnRestListResponse<HistoryUpdateResponse> getProductUpdateHistory(@RequestParam("page") int page,
    @RequestParam("size") int size, @RequestBody HistoryUpdateRequest historyUpdateRequest);

  @RequestMapping(value = "/api/product-level3/v2/{productSku}/item-listing-update",
    method = RequestMethod.POST, produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  GdnBaseRestResponse itemListingUpdateV2(@PathVariable("productSku") String productSku,
    @RequestBody ProductLevel3QuickEditV2Request quickEditV2WebRequests, @RequestParam("isExternalOnly") boolean isExternalOnly);

  @RequestMapping(value = "/api/product-level3/getL5DetailsByProductSku", method = RequestMethod.POST, produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  GdnRestListResponse<ItemPickupPointListingL3Response> getItemPickupPointL3Listing(@RequestParam("page") int page,
      @RequestParam("size") int size, @RequestParam("onlyDefaultViewConfig") boolean onlyDefaultViewConfig,
      @RequestParam("concatenateValueWithValueType") boolean concatenateValueWithValueType,
      @RequestBody ItemPickupPointListingL3Request itemPickupPointListingL3Request);

  @RequestMapping(value = "/api/product-level3/v2/{productSku}/getL3ProductDetailsByProductSku", method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
  GdnRestSingleResponse<ProductL3DetailsResponse> getL3ProductDetailsByProductSku(
      @PathVariable("productSku") String productSku, @RequestParam("isNeedCorrection") boolean isNeedCorrection,
      @RequestParam("concatenateValueWithValueType") boolean concatenateValueWithValueType);

  @RequestMapping(value = "/api/product-level3/v2/{productSku}/fetchL4DetailsByProductSku", method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
  GdnRestListResponse<ItemSummaryL4Response> getL4ProductDetailsByProductSku(
      @PathVariable("productSku") String productSku, @RequestParam("page") int page, @RequestParam("size") Integer size);

  @RequestMapping(value = "/api/product-level3/v2/getL5SummaryByProductSkus",
    method = RequestMethod.POST, produces = MediaType.APPLICATION_JSON_VALUE,
    consumes = MediaType.APPLICATION_JSON_VALUE)
  GdnRestListResponse<ProductLevel3SummaryResponse> getL5SummaryByProductSkuList(
    @RequestParam("page") int page, @RequestParam("size") int size,
    @RequestParam("businessPartnerCode") String businessPartnerCode,
    @RequestBody ProductSkuAndPickupPointCodeRequest productSkuAndPickupPointCodeRequest);

  @RequestMapping(value = "/api/product-level3/filter/bulk-new-download-summary", method = RequestMethod.POST, produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  GdnRestSingleResponse<BulkDownloadProductLevel3Response> bulkDownloadSummaryFromDb(
      @RequestParam("businessPartnerCode") String businessPartnerCode, @RequestBody ProductLevel3SummaryRequest request,
      @RequestParam("fetchProductData") boolean fetchProductData);

  @RequestMapping(value = "/api/product-level3/v2/product/{productSku}/edit-info", method = RequestMethod.POST, consumes = MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
  GdnRestSingleResponse<EditProductV2Response> editProductV2Info(
    @RequestBody com.gdn.partners.pcu.external.client.model.ProductL3UpdateRequest product, @PathVariable("productSku") String productSku,
    @RequestParam("isOnlyExternal") boolean isOnlyExternal);

  @RequestMapping(value = "/api/product-level3/v2/{productSku}/fetchL3Details", method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
  GdnRestSingleResponse<ProductLevel3DetailsV2Response> fetchL3V2ProductDetailsByProductSku(
    @PathVariable("productSku") String productSku,
    @RequestParam("isNeedCorrection") boolean isNeedCorrection);

  @RequestMapping(value = "/api/product/edit-price-stock-variants-info", method = RequestMethod.POST,
      consumes = MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
  GdnRestSingleResponse<ItemsPriceStockImagesUpdateResponse> editL5PriceStockInfo(
      @RequestBody ProductVariantUpdateRequest productVariantUpdateRequest);

  @RequestMapping(value = "/api/product-level3/v2/{businessPartnerCode}/count", method = RequestMethod.GET)
  GdnRestSingleResponse<ProductCountResponse> getProductCountForProductLimit(
      @PathVariable("businessPartnerCode") String businessPartnerCode);

  @RequestMapping(value = "/api/product/{businessPartnerCode}/get-appeal-product-eligibility", method = RequestMethod.GET, consumes = {
    MediaType.APPLICATION_JSON_VALUE})
  GdnRestSingleResponse<AppealProductConfigResponse> getAppealProductConfig(
    @RequestParam String storeId, @RequestParam String requestId,
    @PathVariable("businessPartnerCode") String businessPartnerCode);

  @RequestMapping(value = "/api/distribution-info/{productCode}", method = RequestMethod.POST,
                  produces = MediaType.APPLICATION_JSON_VALUE, consumes =
                      MediaType.APPLICATION_JSON_VALUE)
  GdnBaseRestResponse updateDistributionInfo(@PathVariable("productCode") String productCode,
      @RequestBody DistributionInfoUpdateRequest distributionInfoUpdateRequest);

  @RequestMapping(method = RequestMethod.POST, value = "/api/product/v2/checkIfSellerSkuExists",
      produces = MediaType.APPLICATION_JSON_VALUE)
  GdnRestSingleResponse<OmniChannelMapAndSkuResponse> checkOmniChannelSkuExistsInSeller(
      @RequestBody OmniChannelExistsRequest omniChannelSkuRequest);
}
