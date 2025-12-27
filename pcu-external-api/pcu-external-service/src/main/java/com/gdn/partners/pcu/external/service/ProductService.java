package com.gdn.partners.pcu.external.service;

import com.gda.mta.product.dto.BulkDeleteProductWipRequest;
import com.gda.mta.product.dto.LogAuditTrailUpdatedProductResponse;
import com.gdn.partners.pcu.external.client.model.ProductCreationRequest;
import com.gda.mta.product.dto.ProductLevel3StockInfoWebSiteResponse;
import com.gda.mta.product.dto.ProductLevel3SummaryResponse;
import com.gda.mta.product.dto.ProductLevel3UpdateSummaryRequest;
import com.gda.mta.product.dto.UpdateImageRequest;
import com.gda.mta.product.dto.UpdateProductLevel3InfoRequest;
import com.gda.mta.product.dto.VendorNotesRequest;
import com.gda.mta.product.dto.response.VendorNotesResponse;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.partners.core.web.dto.ListBaseResponse;
import com.gdn.partners.pbp.dto.productlevel3.PostLiveProductCountResponse;
import com.gdn.partners.pbp.model.SortOrder;
import com.gdn.partners.pcu.external.client.model.SubmitEvidenceIPRRequest;
import com.gdn.partners.pcu.external.web.model.request.ActiveProductWebRequest;
import com.gdn.partners.pcu.external.web.model.request.AppealProductWebRequest;
import com.gdn.partners.pcu.external.web.model.request.BulkRequest;
import com.gdn.partners.pcu.external.web.model.request.HistorySummaryWebRequest;
import com.gdn.partners.pcu.external.web.model.request.HistoryUpdateWebRequest;
import com.gdn.partners.pcu.external.web.model.request.InActiveProductWebRequest;
import com.gdn.partners.pcu.external.web.model.request.InProcessProductWebRequest;
import com.gdn.partners.pcu.external.web.model.request.ItemPickupPointListingL3WebRequest;
import com.gdn.partners.pcu.external.web.model.request.ItemLevel4WebRequest;
import com.gdn.partners.pcu.external.web.model.request.NeedRevisionSubmitWebRequest;
import com.gdn.partners.pcu.external.web.model.request.PickupPointUpdateWebRequest;
import com.gdn.partners.pcu.external.web.model.request.PriceChangeCompatibleRequest;
import com.gdn.partners.pcu.external.web.model.request.ProductEditInfoWebRequest;
import com.gdn.partners.pcu.external.web.model.request.ProductImageEditWebRequest;
import com.gdn.partners.pcu.external.web.model.request.ProductL3ListingWebRequest;
import com.gdn.partners.pcu.external.web.model.request.ProductLevel3UpdateWebRequest;
import com.gdn.partners.pcu.external.web.model.request.ProductLevel3VariantsWebRequest;
import com.gdn.partners.pcu.external.web.model.request.ProductLevel3WebRequest;
import com.gdn.partners.pcu.external.web.model.request.ProductPriceAndStockUpdateWebRequest;
import com.gdn.partners.pcu.external.web.model.request.ProductSummaryWebRequest;
import com.gdn.partners.pcu.external.web.model.request.ProductVariantUpdateWebRequest;
import com.gdn.partners.pcu.external.web.model.request.QuickEditWebRequest;
import com.gdn.partners.pcu.external.web.model.request.ReelProductListingWebRequest;
import com.gdn.partners.pcu.external.web.model.request.SuspensionWebRequest;
import com.gdn.partners.pcu.external.web.model.request.UpcStatusWebRequest;
import com.gdn.partners.pcu.external.web.model.request.UpdateItemsPriceStockImagesWebRequest;
import com.gdn.partners.pcu.external.web.model.request.WholeSaleDetailListWebRequest;
import com.gdn.partners.pcu.external.web.model.response.ActiveProductWebResponse;
import com.gdn.partners.pcu.external.web.model.response.AppealProductConfigResponse;
import com.gdn.partners.pcu.external.web.model.response.BrandPredefinedAttributeValueWebResponse;
import com.gdn.partners.pcu.external.web.model.response.BusinessPartnerPickupPointWebResponse;
import com.gdn.partners.pcu.external.web.model.response.CategorySuggestionWebResponse;
import com.gdn.partners.pcu.external.web.model.response.CreateProductResponse;
import com.gdn.partners.pcu.external.web.model.response.EditProductWebResponse;
import com.gdn.partners.pcu.external.web.model.response.EstimateItemPriceWebResponse;
import com.gdn.partners.pcu.external.web.model.response.HistorySummaryWebResponse;
import com.gdn.partners.pcu.external.web.model.response.HistoryUpdateWebResponse;
import com.gdn.partners.pcu.external.web.model.response.InProcessWebResponse;
import com.gdn.partners.pcu.external.web.model.response.InventorySummaryWebResponse;
import com.gdn.partners.pcu.external.web.model.response.InventoryWarehouseStockWebResponse;
import com.gdn.partners.pcu.external.web.model.response.ItemDetailWebResponse;
import com.gdn.partners.pcu.external.web.model.response.ItemL5ListingResponse;
import com.gdn.partners.pcu.external.web.model.response.ItemPickupPointListingL3WebResponse;
import com.gdn.partners.pcu.external.web.model.response.ItemLevel4ListingWebResponse;
import com.gdn.partners.pcu.external.web.model.response.ItemsPriceStockImagesUpdateWebResponse;
import com.gdn.partners.pcu.external.web.model.response.OrderPlacedWebResponse;
import com.gdn.partners.pcu.external.web.model.response.OrderStatusWebResponse;
import com.gdn.partners.pcu.external.web.model.response.PickupPointCodeWebResponse;
import com.gdn.partners.pcu.external.web.model.response.PickupPointUpdateWebResponse;
import com.gdn.partners.pcu.external.web.model.response.PriceChangeCompatibleResponse;
import com.gdn.partners.pcu.external.web.model.response.ProductBasicWebResponse;
import com.gdn.partners.pcu.external.web.model.response.ProductCountWebResponse;
import com.gdn.partners.pcu.external.web.model.response.ProductDetailWebResponse;
import com.gdn.partners.pcu.external.web.model.response.ProductItemDetailWebResponse;
import com.gdn.partners.pcu.external.web.model.response.ProductItemNameWebResponse;
import com.gdn.partners.pcu.external.web.model.response.ProductL3CountWebResponse;
import com.gdn.partners.pcu.external.web.model.response.ProductL3ListingWebResponse;
import com.gdn.partners.pcu.external.web.model.response.ProductLevel3DetailWebResponse;
import com.gdn.partners.pcu.external.web.model.response.ProductLevel3ListingWebResponse;
import com.gdn.partners.pcu.external.web.model.response.ProductLevel3MasterWebResponse;
import com.gdn.partners.pcu.external.web.model.response.ProductLevel3SummaryDetailsWebResponse;
import com.gdn.partners.pcu.external.web.model.response.ProductLevel3SummaryWebResponse;
import com.gdn.partners.pcu.external.web.model.response.ProductScoreRuleWebResponse;
import com.gdn.partners.pcu.external.web.model.response.ProductSystemParameterSwitchWebResponse;
import com.gdn.partners.pcu.external.web.model.response.ReelProductDetailWebResponse;
import com.gdn.partners.pcu.external.web.model.response.SimpleCategoryWebResponse;
import com.gdn.partners.pcu.external.web.model.response.SuspensionWebResponse;
import com.gdn.partners.pcu.external.web.model.response.TemplateDownloadFilePathWebResponse;
import com.gdn.partners.pcu.external.web.model.response.UniquePickupPointCodeWebResponse;
import com.gdn.partners.pcu.external.web.model.response.UpcCodeAndImagesWebResponse;
import com.gdn.partners.pcu.external.web.model.response.UpcStatusWebResponse;
import com.gdn.partners.pcu.external.web.model.response.WholesalePromoResponse;
import com.gdn.partners.pcu.external.web.model.response.YouTubeAPIWebResponse;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;

import jakarta.servlet.http.HttpServletResponse;
import java.util.List;
import java.util.Map;

public interface ProductService {

  /**
   * Get list of estimated price for ItemCode list
   *
   * @param itemCodes
   * @param lowestPriceCoefficient
   * @return List of EstimateItemPriceWebResponse
   */
  List<EstimateItemPriceWebResponse> getEstimatedPriceByItemCodes(List<String> itemCodes, double lowestPriceCoefficient);

  /**
   * Get product details by product Id
   *
   * @param productId
   * @return
   */
  ProductDetailWebResponse getProductDetailsByProductId(String productId);

  /**
   * get product items by name and category codes
   *
   * @param keyword
   * @param categoryCodes
   * @param page
   * @param size
   * @param isOnlyExternal
   * @return
   */
  Page<ProductItemDetailWebResponse> getProductItemsByKeywordAndCategoryCodes(String keyword,
      List<String> categoryCodes, Integer page, Integer size, boolean isOnlyExternal);

  /**
   * get product items by upcCode and category Ids
   *
   * @param upcCode
   * @param categoryIds
   * @param page
   * @param size
   * @param isOnlyExternal
   * @return
   */
  Page<ProductItemDetailWebResponse> getProductItemsByUPCCodeAndCategoryIds(String upcCode,
      List<String> categoryIds, Integer page, Integer size, boolean isOnlyExternal);

  /**
   *
   * @param username
   * @param productCreationRequest
   * @param businessPartnerCode
   * @param flowType
   * @return
   * @throws Exception
   */
  CreateProductResponse createProduct(String username,
      ProductCreationRequest productCreationRequest, String businessPartnerCode, String flowType) throws Exception;

  /**
   *
   * @param username
   * @param productCreationRequest
   * @param businessPartnerCode
   * @param flowType
   * @return
   * @throws Exception
   */
  CreateProductResponse createProductV2(String username,
      ProductCreationRequest productCreationRequest, String businessPartnerCode, String flowType) throws Exception;

  /**
   * Get the top category suggestion for the search keyword
   * @param keyword
   * @param pageable
   * @param requestId
   * @return
   */
  List<List<SimpleCategoryWebResponse>> getTopCategorySuggestions(
      String keyword, Pageable pageable, String requestId);

  /**
   * Get Category suggestion Tree with productCount
   * @param keyword
   * @param pageable
   * @param requestId
   * @param isOnlyExternal
   * @return
   */
  ListBaseResponse<CategorySuggestionWebResponse> getCategorySuggestions(String keyword, Pageable pageable,
      String requestId, boolean isOnlyExternal);

  /**
   * Get product item suggestions by item name and category ID
   *
   * @param itemName
   * @param categoryId
   * @param page
   * @param size
   * @return
   */
  List<ProductItemDetailWebResponse> getProductItemSuggestionsByItemNameAndCategoryId(String itemName,
      String categoryId, Integer page, Integer size);

  /**
   * To get products count by viewable flag
   *
   * @param viewable
   * @return
   */
  int getProductsCount(boolean viewable);

  /**
   * get products by merchant and category code
   *
   * @param activeProductWebRequest
   * @return
   */
  Page<ActiveProductWebResponse> getProductListByMerchantAndCategoryCode(ActiveProductWebRequest activeProductWebRequest,
      String merchantCode);

  /**
   *
   * @param webRequest
   * @param merchantCode
   * @param page
   * @param size
   * @return
   * @throws Exception
   */
  Page<ProductL3ListingWebResponse> getProductListByMerchantAndCategoryCodeV2(
      ProductL3ListingWebRequest webRequest, String merchantCode, int page, int size)
      throws Exception;

  /**
   * get products list to map to reels
   * @param reelProductListingWebRequest
   * @param page
   * @param size
   * @return
   */
  Page<ReelProductDetailWebResponse> getProductListForReels(
      ReelProductListingWebRequest reelProductListingWebRequest, int page, int size);

  /**
   * get suspended items by merchant and category code
   *
   * @param suspensionWebRequest
   * @param pageable
   * @return
   */
  Page<SuspensionWebResponse> getSuspendedItemListByMerchantAndCategoryCode(SuspensionWebRequest suspensionWebRequest,
      Pageable pageable);

  /**
   * get product counts for active, in-progress and in-active products
   *
   * @param type
   * @param merchantCode
   * @return
   */
  ProductCountWebResponse getProductCounts(String type, String merchantCode);

  /**
   * get inActive products by type for a merchant
   * @param request
   * @param pageRequest
   * @param sort
   * @param type
   * @return
   */
  Page<ProductLevel3SummaryResponse> findSummaryByFilter(InActiveProductWebRequest request, PageRequest pageRequest,
      SortOrder sort, String type);

  /**
   * get active product list for a merchant
   *
   * @param request
   * @param pageRequest
   * @return
   */
  Page<ProductLevel3SummaryWebResponse> getActiveProductList(ActiveProductWebRequest request, PageRequest pageRequest);

  /**
   * get in-process product list
   *
   * @param request
   * @param pageRequest
   * @return
   */
  Page<InProcessWebResponse> getInprocessProductList(InProcessProductWebRequest request, PageRequest pageRequest);

  /**
   * get active product name list for a merchant
   *
   * @param searchKey
   * @param merchantCode
   * @param isProductName
   * @param pageRequest
   * @param inStock
   * @return
   */
  Page<ItemDetailWebResponse> getActiveProductNameList(String searchKey, String merchantCode, boolean isProductName,
      PageRequest pageRequest, boolean inStock);

  /**
   * API to archive/unarchive the list of item skus
   *
   * @param itemSkus
   * @param doArchive
   * @param businessPartnerCode
   * @return
   */
  void toggleArchiveItems(List<String> itemSkus, boolean doArchive, String businessPartnerCode);

  /**
   * API to retrive the product update logs
   *
   * @param gdnSku
   * @param pageable
   * @return
   */
  Page<LogAuditTrailUpdatedProductResponse> getProductUpdateLogs(String gdnSku, Pageable pageable);

  /**
   * API to update the item
   *
   * @param gdnSku
   * @param businessPartnerCode
   * @param request
   * @return
   */
  ProductLevel3SummaryResponse updateSummary(String gdnSku, String businessPartnerCode, ProductLevel3UpdateSummaryRequest request);

  /**
   * API to delete the in-progress products
   *
   * @param businessPartnerCode
   * @param request
   * @return
   */
  PostLiveProductCountResponse bulkDeleteProductWip(String businessPartnerCode, BulkDeleteProductWipRequest request);

  /**
   * API to update the item
   *
   * @param businessPartnerCode
   * @param request
   * @return
   */
  Map<String, ProductLevel3SummaryResponse> updateBulkSummary(String businessPartnerCode,
      Map<String, ProductLevel3UpdateSummaryRequest> request);


  /**
   * API to retry activation of the product
   *
   * @param productBusinessPartnerIds
   * @return
   */
  void retryCreate(List<String> productBusinessPartnerIds);

  /**
   * API to download template for selected products
   *
   * @param username
   * @param businessPartnerCode
   * @param isOnlyExternalUser
   * @param bulkRequest
   * @param servletResponse
   * @return
   */
  void downloadTemplateBulkUpdate(String username, String businessPartnerCode, boolean isOnlyExternalUser, BulkRequest bulkRequest, HttpServletResponse servletResponse)
      throws Exception;

  /**
   * API to download update template for selected product Skus
   * @param businessPartnerCode
   * @param isOnlyExternalUser
   * @param productSkuList
   * @param httpServletResponse
   * @param page
   * @param size
   */
  void downloadTemplateBulkUpdateByProductSku(String businessPartnerCode, boolean isOnlyExternalUser, List<String> productSkuList,
      HttpServletResponse httpServletResponse, int page, int size)
      throws Exception;

  /**
   * @param businessPartnerCode
   * @param httpServletResponse
   * @throws Exception
   */
  void downloadTemplateForMultiPickupPointTemplate(String businessPartnerCode, HttpServletResponse httpServletResponse)
      throws Exception;

  /**
   * API to get moving price from cogs value for multivariant products
   *
   * @param materialCode
   * @param businessPartnerCode
   * @return
   */
  Double getCogsValue(String materialCode, String businessPartnerCode);

  /**
   * API to download all products
   *
   * @param username
   * @param isOnlyExternalUser
   * @param businessPartnerCode
   * @return
   */
  void downloadAllProduct(String username, boolean isOnlyExternalUser, String businessPartnerCode,
      ProductSummaryWebRequest request) throws Exception;

  void downloadAllProductWithEAN(String username, boolean isOnlyExternalUser, String businessPartnerCode,
                          ProductSummaryWebRequest request) throws Exception;

  /**
   * API to get stock info website
   *
   * @param merchantCode
   * @param itemSku
   * @return
   */
  ProductLevel3StockInfoWebSiteResponse getStockInfoWebSite(String merchantCode, String itemSku);

  /**
   * API to download template for particular category
   *
   * @param username
   * @param businessPartnerCode
   * @param categoryId
   * @param servletResponse
   * @param isOnlyExternal
   * @return
   */
  void downloadProductTemplate(String username, String businessPartnerCode, String categoryId,
      HttpServletResponse servletResponse, boolean isOnlyExternal) throws Exception;


  /**
   * API to check the sync stock mode and product permission for merchant
   *
   * @param merchantCode
   * @return
   */
  boolean checkSyncStockModeAndProductPermission(String merchantCode);

  /**
   * API to update product detail by external user
   *
   * @param request
   * @param businessPartnerCode
   * @param isOnlyExternal
   * @return
   */
  void updateProductDetails(ProductLevel3WebRequest request, String businessPartnerCode, String isOnlyExternal)
      throws ApplicationRuntimeException;


   /** API to get details of productLevel4
   *
   * @param businessPartnerCode
   * @param gdnSku
   * @return
   */
   ProductLevel3MasterWebResponse findDetailByGdnSku(String businessPartnerCode, String gdnSku);

  /**
   * API to unsynchronize product having productSku and itemSku
   *
   * @param businessPartnerCode
   * @param productSku
   * @param itemSku
   * @return
   */
  void unsynchronizeProduct(String businessPartnerCode, String productSku, String itemSku);

  /**
   * API to synchronize product having productSku and itemSku
   *
   * @param businessPartnerCode
   * @param productSku
   * @param itemSku
   * @return
   */
  void synchronizeProduct(String businessPartnerCode, String productSku, String itemSku);

  /**
   * API to update image
   *
   * @param request
   * @param isOnlyExternal
   * @return
   */
  void updateProductImage(UpdateImageRequest request, boolean isOnlyExternal) throws Exception;

  /**
   * API to validate youtube url
   *
   * @param youTubeUrl
   * @return
   */
  YouTubeAPIWebResponse validateYouTubeUrl(String youTubeUrl) throws Exception;

  /**
   * API to fetch minimum price
   *
   * @return
   */
  Integer getMinimumPrice();

  /**
   * get brand predefined allowed attribute value
   *
   * @param brandCode
   * @param brandStatus
   * @return
   */
  List<BrandPredefinedAttributeValueWebResponse> getBrandPredefinedAllowedAttributeValueDetail(String brandCode,
      String brandStatus);

  /**
   * Api to validate the price bulk update
   *
   * @param priceChangeCompatibleRequest
   * @return
   */
  List<PriceChangeCompatibleResponse> getPriceChangeCompatibility(String storeId, String requestId,
      List<PriceChangeCompatibleRequest> priceChangeCompatibleRequest) throws Exception;


  /**
   * Get wholesale promo status for list of item Skus
   *
   * @param storeId
   * @param requestId
   * @param wholeSaleDetailListWebRequests
   * @return
   */
  List<WholesalePromoResponse> getWholesalePromoStatus(String storeId, String requestId, List<WholeSaleDetailListWebRequest>
      wholeSaleDetailListWebRequests);


  /**
   * Update price and stock of an item by itemSku
   *
   * @param productPriceAndStockUpdateWebRequest
   * @param businessPartnerCode
   * @return
   */
  boolean updatePriceAndStock(ProductPriceAndStockUpdateWebRequest productPriceAndStockUpdateWebRequest,
      String businessPartnerCode);

  /**
   *
   * @param categoryCode
   * @return
   */
  ProductScoreRuleWebResponse getProductScoreRule(String categoryCode);

  /**
   * Get switch values from PBP system parameter
   *
   * @return
   */
  ProductSystemParameterSwitchWebResponse getSystemParameterSwitches();

  /**
   * Get upcCode and images of all L4 mapped to L3
   *
   * @return
   */
  UpcCodeAndImagesWebResponse getUpcCodeAndImages(String storeId, String requestId, String productSku);

  /**
   * API to edit product info by external user
   *
   * @param request
   * @param businessPartnerCode
   * @param isOnlyExternal
   * @return
   */
  EditProductWebResponse editProductInfo(ProductEditInfoWebRequest request, String businessPartnerCode, boolean isOnlyExternal)
      throws ApplicationRuntimeException;

  /**
   * @param request
   * @param pageRequest
   * @return
   */
  Page<ProductLevel3SummaryDetailsWebResponse> getProductLevel3VariantList(ProductLevel3VariantsWebRequest request,
      PageRequest pageRequest);

  /**
   *
   * @param productSku
   * @return
   * @throws Exception
   */
  OrderPlacedWebResponse checkSuccessfullOrderPlacedForProductSku(String productSku) throws Exception;

  /**
   * Return list of pickup point codes comma separated which don't have pinpoint location
   *
   * @param productSku
   * @param businessPartnerCode
   * @return
   */
  String getPinpointStatusByProductSku(String productSku, String businessPartnerCode);

  /**
   * Get pickupPointCodes of L4s under a L3 in a paginated form
   * @param page
   * @param size
   * @param productSku
   * @param needCorrection
   * @param businessPartnerCode
   * @param fbbActivated
   * @return
   * @throws Exception
   */
  Page<PickupPointCodeWebResponse> getPickupPointCodesByProductSku(int page, int size, String productSku,
      boolean needCorrection, String businessPartnerCode, boolean fbbActivated) throws Exception;

  /**
   * Get all distinct pickupPointCodes for a L3
   * @param productSku
   * @return
   * @throws Exception
   */
  UniquePickupPointCodeWebResponse getUniquePickupPointCodesByProductSku(String productSku)
      throws Exception;

  /**
   * @param businessPartnerCode
   * @param updateItemsPriceStockImagesWebRequest
   * @return
   */
  ItemsPriceStockImagesUpdateWebResponse updateItemsPriceStockImages(String businessPartnerCode,
      UpdateItemsPriceStockImagesWebRequest updateItemsPriceStockImagesWebRequest);

  /**
   * api to fetch the variant history summary
   * @param historySummaryWebRequest
   * @param page
   * @param size
   * @return
   */
  Page<HistorySummaryWebResponse> getProductEditHistorySummary(HistorySummaryWebRequest historySummaryWebRequest,
      int page, int size);

  /**
   * Update the logistics details
   * @param productLevel3UpdateWebRequest
   * @param businessPartnerCode
   * @param isNeedCorrection
   * @return
   * @throws Exception
   */
  void updateLogistics(ProductLevel3UpdateWebRequest productLevel3UpdateWebRequest, String businessPartnerCode,
      String isExternalOnly, boolean isNeedCorrection) throws Exception;

  /**
   * Get product order status by productCode
   *
   * @return
   */
  OrderStatusWebResponse getOrderStatusByProductCode(String productCode);

  /**
   * API to update the pick-up points
   *
   * @param pickupPointUpdateWebRequest
   * @param businessPartnerCode
   * @return
   */
  PickupPointUpdateWebResponse updatePickupPoints(PickupPointUpdateWebRequest pickupPointUpdateWebRequest, String businessPartnerCode) throws Exception;

  /**
   * @param productSku
   * @param page
   * @param size
   * @return
   */
  Page<ProductItemNameWebResponse> getProductVariantsNameByProductSku(String productSku, int page, int size);

  /**
   * API to fetch the detail by product sku.
   *
   * @param productSku
   * @param businessPartnerCode
   * @param isNeedCorrection
   * @return
   */
  ProductLevel3DetailWebResponse getL3DetailByProductSku(String productSku, String businessPartnerCode,
      boolean isNeedCorrection) throws Exception;

  /**
   * API to fetch inventory summary for reserved stock.
   *
   * @param itemSku
   * @param isWareHouse
   * @param merchantCode
   * @return
   */
  InventorySummaryWebResponse getInventorySummary(String itemSku, boolean isWareHouse, String merchantCode)
      throws Exception;

  /**
   * get the l3 product counts for active, in-progress and in-active products
   *
   * @param type
   * @param merchantCode
   * @return
   */
  ProductL3CountWebResponse getL3ProductCounts(String type, String merchantCode);

  /**
   * l4 listing update api
   * @param productSku
   * @param quickEditWebRequests
   */
  void updateItemListing(String productSku, List<QuickEditWebRequest> quickEditWebRequests) throws Exception;

  /**
   * API to archive list of product skus.
   *
   * @param productSkus
   * @param doArchive
   * @param businessPartnerCode
   * @return
   */
  void toggleArchiveProducts(List<String> productSkus, boolean doArchive, String businessPartnerCode);

  /**
   * api to fetch product l3 list
   * @param productSummaryRequest
   * @param page
   * @param size
   * @return
   */
  Page<ProductLevel3ListingWebResponse> getProductL3List(ProductSummaryWebRequest productSummaryRequest, int page,
      int size);


  /**
   * get items by product sku and business partner code
   *
   * @param productSku
   * @param businessPartnerCode
   * @param pageRequest
   * @return
   */
  Page<ProductLevel3SummaryWebResponse> getItemsByProductSku(String productSku, String businessPartnerCode,
      PageRequest pageRequest);

  /**
   * API to get vendor notes.
   *
   * @param productCode
   * @return
   */
  VendorNotesResponse getVendorNotes(String productCode);

  /**
   * API to update vendor notes.
   *
   * @param productCode
   * @param vendorNotesRequest
   * @return
   */
  void updateVendorNotes(String productCode, VendorNotesRequest vendorNotesRequest);

  /**
   * Api to re-submit corrected product
   * @param needRevisionSubmitWebRequest
   * @return
   */
  EditProductWebResponse submitNeedForRevisionProduct(NeedRevisionSubmitWebRequest needRevisionSubmitWebRequest);

  /**
   * Appeal products from in-progress tab
   * @param appealProductWebRequest
   * @return
   */
  boolean appealProductsInProgress(AppealProductWebRequest appealProductWebRequest,
      String businessPartnerCode);

  /**
   *
   * @param request
   * @param businessPartnerCode
   * @param isOnlyExternal
   * @param productSku
   * @return
   */
  EditProductWebResponse updateProductInfo(UpdateProductLevel3InfoRequest request, String businessPartnerCode,
      boolean isOnlyExternal, String productSku) throws Exception;

  /**
   * fetch item names from x-product using item sku
   * @param itemSkus
   * @return
   */
  Map<String, String> fetchItemNamesByItemSku(List<String> itemSkus);

  /**
   * Api to update images
   *
   * @param productSku
   * @param businessPartnerCode
   * @param productImageEditWebRequest
   * @return
   */
  ItemsPriceStockImagesUpdateWebResponse updateImages(String productSku, String businessPartnerCode,
      ProductImageEditWebRequest productImageEditWebRequest) throws Exception;
  /**
   * Api to Fetch L4 listing response from x-product using a list of item Skus
   *
   * @param storeId
   * @param request
   * @return
   */
  Page<ItemLevel4ListingWebResponse> getL4ItemListByProductSku(String storeId, String requestId, int page, Integer size,  ItemLevel4WebRequest request);

  /**
   * Fetch product update history by input request and pagination
   *
   * @param historyUpdateWebRequest
   * @param page
   * @param size
   * @return
   * @throws Exception
   */
  Page<HistoryUpdateWebResponse> getProductUpdateHistory(
    HistoryUpdateWebRequest historyUpdateWebRequest, int page, int size) throws Exception;

  /**
   * get itemPickupPoint listing by productSku
   *
   * @param onlyDefaultViewConfig
   * @param productSku
   * @param itemPickupPointListingL3WebRequest
   * @param concatenateValueWithValueType
   * @return
   */
  Page<ItemPickupPointListingL3WebResponse> getItemPickupPointListingByProductSku(int page, int size,
      boolean onlyDefaultViewConfig, String productSku,
      ItemPickupPointListingL3WebRequest itemPickupPointListingL3WebRequest, boolean concatenateValueWithValueType);

  /**
   * Fetch  business partner pickup point details by pick up point codes
   *
   * @param pickupPointCodes
   * @return
   * @throws Exception
   */
  List<BusinessPartnerPickupPointWebResponse> getPickupDetailByCode(List<String> pickupPointCodes, String merchantCode)
    throws Exception;

  /**
   * Edit L5 price and stock
   *
   * @param productVariantUpdateWebRequest
   * @param businessPartnerCode
   * @return
   */
  ItemsPriceStockImagesUpdateWebResponse editL5ProductStockAndPrice(
      ProductVariantUpdateWebRequest productVariantUpdateWebRequest, String businessPartnerCode)
      throws Exception;

  /**
   *to get all the external template download file paths
   *
   * @return
   */
  TemplateDownloadFilePathWebResponse getExternalDownloadTemplateFilePaths();

  /**
   * Get item basic details
   *
   * @param productSku
   * @param fetchAllDetails attaches info from PCB to API response, if true
   * @return
   */
  List<ItemDetailWebResponse> getItemBasicDetails(String productSku, boolean fetchAllDetails);

  /**
   * Get basic item details for item skus
   *
   * @param itemSkus
   * @param fetchBundleRecipe
   * @return
   */
  List<ItemDetailWebResponse> getItemBasicDetails(List<String> itemSkus, boolean fetchBundleRecipe) throws Exception;

  /**
   * Get item L5 details
   *
   * @param page         must not be null
   * @param size         must not be null
   * @param productSku   must not be blank
   * @param cncActivated must not be null
   * @return
   */
  Page<ItemL5ListingResponse> getItemL5Details(String productSku, Integer page, Integer size,
      Boolean cncActivated, boolean fetchOnlyBundleVariants);

  /**
   * Get warehouse stock info
   *
   * @param warehouseCode
   * @param itemSkus
   * @return
   */
  List<InventoryWarehouseStockWebResponse> getWarehouseStockByItemSkusAndWarehouseCode(String warehouseCode,
      List<String> itemSkus);

  /**
   * download templates for assembly,dis-assembly,transfer according to type
   *
   * @param type
   * @param response
   * @throws Exception
   */
  void downloadWorkOrderTemplates(String type, HttpServletResponse response) throws Exception;

  /**
   * Return threshold count & present limit
   *
   * @param storeId             storeId
   * @param requestId           storeId
   * @param businessPartnerCode business partner code
   * @return
   */
  AppealProductConfigResponse fetchAppealProductConfig(String storeId, String requestId,
    String businessPartnerCode);

  /**
   * Submit evidence for IPR products
   *
   * @param submitEvidenceIPRRequest
   */
  void submitEvidenceForIPR(SubmitEvidenceIPRRequest submitEvidenceIPRRequest);

  /**
   * Return if upcCode exits in seller
   * @param upcStatusWebRequest upcStatusWebRequest
   * @return
   */
  List<UpcStatusWebResponse> getUpcStatus(UpcStatusWebRequest upcStatusWebRequest);

  /**
   * republishProductToAgp
   *
   * @param productSku
   */
  void republishProductToAgp(String productSku);

  /**
   * Republish item to AGP
   *
   * @param itemSku
   */
  void republishItemToAgp(String itemSku);

  /**
   * Republish l5 event
   *
   * @param itemSku
   * @param pickupPointCode
   * @param republishToAgp
   */
  void republishItemPickupPointToAgp(String itemSku, String pickupPointCode, boolean republishToAgp);

  /**
   * Get product basic details by product skus
   * @param productSkus
   * @return
   * @throws Exception
   */
  List<ProductBasicWebResponse> getProductBasicDetailsByProductSkus(List<String> productSkus)
      throws Exception;

  /**
   * API to download all products
   *
   * @param username String
   * @param businessPartnerCode String
   * @param request ProductSummaryWebRequest
   */
  void downloadAllProductBasicInfo(String username, String businessPartnerCode, ProductSummaryWebRequest request);

  /**
   * Reindex brand collection by brand request code
   *
   * @param brandRequestCode String
   */
  void reindexBrandCollection(String brandRequestCode);
}
