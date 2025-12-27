package com.gdn.mta.product.service;

import java.util.Date;
import java.util.List;
import java.util.Map;

import com.gda.mta.product.dto.ItemImageUpdateRequestPCB;
import com.gda.mta.product.dto.PickupPointDeleteRequest;
import com.gda.mta.product.dto.ProductDetailEditDTO;
import com.gda.mta.product.dto.ProductEditContext;
import com.gda.mta.product.dto.response.InProgressProductsBySizeChartCodeResponse;
import com.gdn.mta.product.commons.constant.DimensionHolder;
import com.gdn.mta.product.enums.AutoApprovalType;
import com.gdn.x.product.rest.web.model.request.ProductEditRequest;
import com.gdn.x.productcategorybase.dto.response.CategoryResponse;
import com.gdn.x.productcategorybase.dto.response.NewlySavedItemResponse;
import org.apache.commons.lang3.tuple.Pair;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;

import com.gda.mta.product.dto.BrandAndCategoryItemSummaryRequest;
import com.gda.mta.product.dto.BulkDownloadProductLevel3Response;
import com.gda.mta.product.dto.EditProductResponse;
import com.gda.mta.product.dto.EditedResizeAndImagesUpdateStatusResponse;
import com.gda.mta.product.dto.EditedResizeImagesResponse;
import com.gda.mta.product.dto.FbbAndCncDataChangeDto;
import com.gda.mta.product.dto.PickupPointUpdateRequest;
import com.gda.mta.product.dto.PickupPointUpdateResponse;
import com.gda.mta.product.dto.PostLiveFlagsDTO;
import com.gda.mta.product.dto.ProductImageEditRequest;
import com.gda.mta.product.dto.ProductL3SummaryRequest;
import com.gda.mta.product.dto.ProductL3UpdateRequest;
import com.gda.mta.product.dto.ProductLevel3DTO;
import com.gda.mta.product.dto.ProductLevel3QuickEditRequest;
import com.gda.mta.product.dto.ProductLevel3SummaryDetailsImageRequest;
import com.gda.mta.product.dto.ProductLevel3UpdateRequest;
import com.gda.mta.product.dto.ProductLevel3ViewConfigStockRequest;
import com.gda.mta.product.dto.ProductPriceAndWholesaleRequest;
import com.gda.mta.product.dto.ProductPriceStockAndImagesRequest;
import com.gda.mta.product.dto.ProductVariantUpdateRequest;
import com.gda.mta.product.dto.SummaryFilterRequest;
import com.gda.mta.product.dto.SuspensionProductRequest;
import com.gda.mta.product.dto.UpdateItemsPriceStockImagesRequest;
import com.gda.mta.product.dto.UpdateProductLevel3InfoRequest;
import com.gda.mta.product.dto.response.CogsValueResponse;
import com.gda.mta.product.dto.response.InProgressProductsByPickupPointCodeResponse;
import com.gda.mta.product.dto.response.ItemBulkArchiveResponse;
import com.gda.mta.product.dto.response.ItemsPriceStockImagesUpdateResponse;
import com.gda.mta.product.dto.response.ProductItemNameResponse;
import com.gda.mta.product.dto.response.ProductL3SummaryResponse;
import com.gda.mta.product.dto.response.ProductLevel3DetailResponse;
import com.gda.mta.product.dto.response.ProductSuspensionHistoryResponse;
import com.gda.mta.product.dto.response.SuspensionProductResponse;
import com.gdn.mta.product.entity.BulkDownloadProductLevel3Summary;
import com.gdn.mta.product.entity.PickupPointCodeResponse;
import com.gdn.mta.product.entity.ProductBusinessPartner;
import com.gdn.mta.product.entity.ProductCollection;
import com.gdn.mta.product.entity.ProductItemsCogs;
import com.gdn.mta.product.entity.ProductLevel3;
import com.gdn.mta.product.entity.ProductLevel3Attribute;
import com.gdn.mta.product.entity.ProductLevel3Dashboard;
import com.gdn.mta.product.entity.ProductLevel3Logistics;
import com.gdn.mta.product.entity.ProductLevel3Order;
import com.gdn.mta.product.entity.ProductLevel3Summary;
import com.gdn.mta.product.entity.ProductLevel3SummaryDetails;
import com.gdn.mta.product.entity.ProductLevel3UpdateSummary;
import com.gdn.mta.product.entity.ProductSuspensionHistory;
import com.gdn.mta.product.entity.UniquePickupPointCodeResponse;
import com.gdn.mta.product.entity.UsageStatus;
import com.gdn.mta.product.enums.ApiErrorCode;
import com.gdn.mta.product.enums.SuspensionStatus;
import com.gdn.mta.product.valueobject.EstimateItemPriceDTO;
import com.gdn.mta.product.valueobject.ProductLevel3SummaryFilter;
import com.gdn.mta.product.valueobject.ProductLevel3SummaryFilterDetails;
import com.gdn.mta.product.valueobject.SortOrder;
import com.gdn.partners.pbp.dto.offlineitem.UpsertOfflineItemFailedResponse;
import com.gdn.partners.pbp.dto.productlevel3.CountProductLevel3InactiveResponse;
import com.gdn.partners.pbp.dto.productlevel3.SuspensionItemResponse;
import com.gdn.partners.pbp.model.offlineitem.UpsertOfflineItem;
import com.gdn.x.businesspartner.dto.ProfileResponse;
import com.gdn.x.product.rest.web.model.dto.ItemSummaryResponse;
import com.gdn.x.product.rest.web.model.request.ItemRequest;
import com.gdn.x.product.rest.web.model.request.ItemViewConfigRequest;
import com.gdn.x.product.rest.web.model.request.ProductRequest;
import com.gdn.x.product.rest.web.model.response.ItemResponse;
import com.gdn.x.product.rest.web.model.response.ProductAndItemsResponse;
import com.gdn.x.product.rest.web.model.response.ProductL3Response;
import com.gdn.x.product.rest.web.model.response.ProductResponse;
import com.gdn.x.productcategorybase.dto.LocationPathAndCommonImage;
import com.gdn.x.productcategorybase.dto.request.ProductItemAttributeValueRequest;
import com.gdn.x.productcategorybase.dto.request.ProductItemImageUpdateRequest;
import com.gdn.x.productcategorybase.dto.response.PredefinedAllowedAttributeValueResponse;
import com.gdn.x.productcategorybase.dto.response.ProductCategoryResponse;
import com.gdn.x.productcategorybase.dto.response.ProductDetailResponse;
import com.gdn.x.productcategorybase.entity.Product;

/**
 * Move to class {@link com.gdn.partners.pbp.service.productlevel3.ProductLevel3Service}
 *             . Please as much as possible add new method in new class.
 */
public interface ProductLevel3Service {
  /**
   * method to the Inprogress products by bpCode and ppCode
   * @param storeId
   * @param businessPartnerCode
   * @param pickupPointCode
   * @param page
   * @param size
   * @return
   */
   Page<InProgressProductsByPickupPointCodeResponse> getInProgressProductsByBusinessPartnerAndPickupPointCode(
      String storeId, String businessPartnerCode, String pickupPointCode, int page, int size);

  /**
   * Get IN_PROGRESS products by sizeChartCode
   *
   * @param storeId
   * @param sizeChartCode
   * @param page
   * @param size
   * @return
   */
  Page<InProgressProductsBySizeChartCodeResponse> getInProgressProductsBySizeChartCode(String storeId,
      String sizeChartCode, int page, int size);

  ProductLevel3 findDetailByGdnSku(String businessPartnerCode, String gdnSku, boolean concatValueAndValueTypes) throws Exception;

  ProductLevel3 findDetailByGdnSkuForAllItemSkus(String businessPartnerCode, String gdnSku) throws Exception;

  /**
   * API to check whether the category is a pristine category or not
   *
   * @param categoryId
   * @throws Exception
   */
  boolean isPristineCategory(String categoryId) throws Exception;

  ProductLevel3Order findDetailOrderByGdnSku(String businessPartnerCode, String gdnSku)
      throws Exception;

//  /**
//   *  repository method to find items minified summary by applying filter as an ItemSummaryResponse
//   *
//   * @param businessPartnerCode merchant Code
//   * @param itemName Name of the item
//   * @param gdnSkus List of gdnSkus
//   * @param categoryCode Category Code filter
//   * @param salePrice Sale Price filter
//   * @param pickupPointCode PickupPoint filter
//   * @param display Displayabe flag filter
//   * @param buyable Buyable flag filter
//   * @param isArchived Archived flag filter
//   * @return Page<ItemSummaryResponse>
//   * @throws Exception
//   */
//  Page<ProductLevel3SummaryMinified> findSummaryMinifiedByFilter(String businessPartnerCode,
//      String itemName, String gdnSkus, String categoryCode, Double salePrice,
//      String pickupPointCode, Boolean display, Boolean buyable, Integer stock,
//      PageRequest pageRequest, Boolean isArchived)
//      throws Exception;

//  Page<ProductLevel3Summary> findSummaryByFilter(ProductLevel3SummaryFilter filterRequest,
//      PageRequest pageRequest, SortOrder sort) throws Exception;

  Page<ProductLevel3Summary> findSummaryByCategoryAndBrandFilter(BrandAndCategoryItemSummaryRequest summaryRequest,
      PageRequest pageRequest, SortOrder sort) throws Exception;

//  Page<ProductLevel3Summary> findSummaryByArchivedFilter(ProductLevel3SummaryFilter filterRequest,
//      PageRequest pageRequest) throws Exception;

  ApiErrorCode updateItemPrice(String storeId, ProductPriceAndWholesaleRequest request, String itemSku) throws Exception;

  /**
   * Update Product Dimensions on Edit
   * @param product DimensionHolder Type to hold dimensions for both ProductLevel3 andMasterDataUpdateRequest
   * @param dimensionsUpdated `true` if dimensions are updated
   * @throws Exception when failed to update
   */
  void updateDimensionsAtL3(DimensionHolder product, boolean dimensionsUpdated) throws Exception;

  /**
   * Update ProductCollection And Solr And Publish History Event
   *
   * @param productName        product name updated
   * @param productCollection
   * @param reviewTypeList
   * @param productEditContext context of the product edit
   *                           (ProductDetailEditDTO and MasterProductEditDTO)
   * @param oldBrandName
   * @throws
   */

  void updateProductCollectionAndSolrAndPublishHistoryEvent(String productName,
    ProductCollection productCollection, List<String> reviewTypeList,
    ProductEditContext productEditContext, String oldBrandName) throws Exception;

  void updateXProductAndPBPL3OnTakeDown(ProductLevel3 product, ProductLevel3 productLevel3,
    ProductDetailEditDTO productDetailEditDTO, ProductL3Response savedProductData,
    ProductEditRequest productEditRequest, boolean isCategoryUpdated) throws Exception;

  /**
   * copy ProductLevel3 To Product And Update To PCB
   *
   * @param productCollection      non null
   * @param productLevel3          ProductLevel3
   * @param isKeywordPresent       boolean
   * @param autoApproved           boolean
   * @param categoryResponse       category Response
   * @param productDetailEditDTO   productDetailEditDTO
   * @param savedDataResponse
   * @param productL3UpdateRequest
   * @throws
   */
  Pair<com.gdn.x.productcategorybase.dto.request.ProductRequest, List<NewlySavedItemResponse>> copyProductLevel3ToProductAndUpdateToPCB(
    ProductCollection productCollection, ProductLevel3 productLevel3, boolean contentChanged,
    boolean isKeywordPresent, boolean autoApproved, CategoryResponse categoryResponse,
    EditProductResponse editProductResponse, ProductDetailEditDTO productDetailEditDTO,
    ProductL3Response savedDataResponse, ProductL3UpdateRequest productL3UpdateRequest) throws Exception;

  /**
   * process auto approval on Product master Data Edit
   * @param productCollection ProductCollection entity
   * @param categoryCode product category code
   * @param updatedBy user who updated the product
   * @return Pair of AutoApprovalType and List of CategoryResponse
   */
  Pair<AutoApprovalType, List<CategoryResponse>> processAutoApproval(
    ProductCollection productCollection, String categoryCode, String updatedBy) throws Exception;

  void publishInternalProductHistoryEvent(ProductCollection productCollection, String userName);

  /**
   * create Audit Logs For Edit
   * @param savedData ProductLevel3DetailResponse
   * @param accessChannel access channel of the edit
   * @param sizeChartChanged size chart changed flag
   * @throws
   */
  void createAuditLogsForEdit(ProductLevel3DetailResponse savedData, String accessChannel, boolean sizeChartChanged) throws Exception;

  void updateItem(ItemRequest request, boolean isOnlyExternal) throws Exception;

  /**
   * Update Discount Price in X-Campaign
   * @param itemSku
   * @param offerPrice
   * @param categoryCode
   * @param pickupPointCode
   * @throws
   */
  void updateDiscountPriceInCampaign(String itemSku, Double offerPrice, String categoryCode, String pickupPointCode)
      throws Exception;

  void updateItemViewConfig(ItemViewConfigRequest itemViewConfigRequest, String itemSku, String username)
      throws Exception;

  /**
   * Changes for an itemSku's itemViewConfig only if there are any changes
   *
   * @param itemViewConfigRequest
   * @param itemSku
   * @param username
   * @throws Exception
   */
  boolean updateItemViewConfigToHideItemSku(ItemViewConfigRequest itemViewConfigRequest, String itemSku,
      String username)
      throws Exception;

  /**
   * Update stock details by itemSku
   *
   * @param businessPartnerCode
   * @param gdnSku
   * @param deltaStock
   * @param minimumStock
   */
  void updateItemStock(String businessPartnerCode, String gdnSku, Integer deltaStock, Integer minimumStock)
      throws Exception;

  void updateItemOff2On(Boolean off2OnActiveFlag, String itemSku, ItemSummaryResponse productData)
      throws Exception;

  void updateItemOff2On(Boolean off2OnActiveFlag, String itemSku) throws Exception;

  /**
   * synchronize the product
   * @param productSku
   * @param itemSku
   * @throws Exception
   */
  ApiErrorCode synchronizeProduct(String productSku, String itemSku) throws Exception;

  /**
   * unsynchronize the product
   * @param productSku
   * @param itemSku
   * @throws Exception
   */
  ApiErrorCode unsynchronizeProduct(String productSku, String itemSku) throws Exception;

  void toggleArchiveItem(String itemSku, boolean doArchive) throws Exception;

  /**
   * create product level 3
   * 
   * @param businessPartnerCode business partner code that own the product
   * @param product product information
   * @param productBusinessPartner product business partner information
   * @throws Exception when failed to create
   * @deprecated dont use this method
   */
  @Deprecated
  void create(String businessPartnerCode, Product product,
      ProductBusinessPartner productBusinessPartner) throws Exception;

  ProductLevel3DTO update(ProductLevel3 product, Integer deltaStock, Integer minimumStock, boolean isOnlyExternal, boolean hasOrder, boolean updateLogistics) throws Exception;

  ProductLevel3DTO update(ProductLevel3 product, Integer deltaStock, Integer minimumStock, boolean isOnlyExternal) throws Exception;

  void update(ProductRequest product, boolean isOnlyExternal) throws Exception;

  ItemResponse getItem(String itemSku) throws Exception;

  ProductAndItemsResponse getProduct(String productSku) throws Exception;

  ProductLevel3Summary updateSummary(String businessPartnerCode, String gdnSku,
      ProductLevel3UpdateSummary request) throws Exception;

  ApiErrorCode productQuickEdit(String storeId, String productSku,
      ProductLevel3QuickEditRequest productLevel3QuickEditRequest, List<String> stockUpdateFailedItemSkus) throws Exception;

  ProductLevel3Summary findSummaryByGdnSku(String businessPartnerCode, String gdnSku)
      throws Exception;

  ProductLevel3Dashboard findDashboard(String businessPartnerCode) throws Exception;

  boolean create(String businessPartnerCode, ProductDetailResponse productData,
      ProductBusinessPartner productBusinessPartner, boolean takeDownProduct,
      List<ProductLevel3Logistics> logistics) throws Exception;

  UsageStatus checkPickupPointCodeUsed(String pickupPointCode) throws Exception;

  /**
   * Get Product summary at level 3 for given business Partner and list of gdnSku
   *
   * @param businessPartnerCode business partner code who own the product
   * @param gdnSkuList list of gdn sku
   * @param pageRequest page information
   * @return page of product level 3
   * @throws Exception when failed to get summary
   */
  Page<ProductLevel3Summary> findSummaryByGdnSkuList(String businessPartnerCode,
      List<String> gdnSkuList, PageRequest pageRequest) throws Exception;

  /**
   * Get Active Product summary at level 3 for given business Partner
   *
   * @param businessPartnerCode business partner code that own products
   * @param pageRequest page information
   * @param requestId request id
   * @param itemName
   * @param gdnSku
   * @param categoryCode
   * @param salePrice
   * @param pickupPointCode
   * @param stock
   * @return BulkDownloadProductLevel3Summary
   * @throws Exception when failed to find product
   */
  BulkDownloadProductLevel3Summary findProductSummaryForBulkDownload(String businessPartnerCode,
      PageRequest pageRequest, String requestId, String itemName, String gdnSku,
      String categoryCode, Double salePrice, String pickupPointCode, Integer stock,
      boolean isArchived) throws Exception;


  /**
   * Get Active Product summary at level 3 for given business Partner
   *
   * @param businessPartnerCode business partner code that own products
   * @param pageRequest page information
   * @param requestId request id
   * @param filterRequest
   * @return BulkDownloadProductLevel3Summary
   * @throws Exception when failed to find product
   */
  BulkDownloadProductLevel3Summary findProductSummaryForBulkDownload(String businessPartnerCode,
      PageRequest pageRequest, String requestId, ProductLevel3SummaryFilter filterRequest) throws Exception;

  /**
   * @param filterRequest
   * @param fetchB2bData
   * @param fetchViewConfigByChannel
   * @return
   * @throws Exception
   */
  BulkDownloadProductLevel3Response findProductSummaryForBulkDownloadByDb(ProductLevel3SummaryFilter filterRequest,
      boolean fetchB2bData, String fetchViewConfigByChannel) throws Exception;

  Long getProductsCountByBrand(String brand) throws Exception;

  /**
   * Estimate normal price and offer price for merchants creating product from flow 2
   * @see <a href="https://jira.gdn-app.com/browse/SHEN-267"</a>
   * for more datils
   * @see <a href="https://jira.gdn-app.com/browse/SHEN-183"</a>
   *
   * @param itemCode itemCode for which price to be estimated
   * @param lowestPriceCoefficient factor decides offer price selection, default = 1.5
   * @param maxInventoryRequest no of items in inventory request
   * @return Estimateed Item Normal, offer price DTO
   * @throws Exception
   */
  EstimateItemPriceDTO estimatePriceForFlow2ProductCreation(String itemCode,
      double lowestPriceCoefficient, int maxInventoryRequest) throws Exception;

  /**
   * fetch ProductAndItemsResponse by productSku
   * @param productSku productSku
   * @return ProductAndItem Response Object
   * @throws Exception
   */
  ProductAndItemsResponse findDetailByProductSku(String productSku) throws Exception;

  void archiveProductStockAlert(String businessPartnerCode, String gdnSku, boolean archived, int retry) throws Exception;

  /**
   * bulk archive item skus
   * @param itemSkus
   * @param businessPartnerCode
   * @return
   */
  List<String> bulkArchiveItems(List<String> itemSkus, String businessPartnerCode);


  /**
   * Get all active brands by cn category id
   *
   * @param requestId
   * @param userName
   * @param categoryId
   * @param clearCache
   * @return
   * @throws Exception
   */
  List<PredefinedAllowedAttributeValueResponse> getAllActiveBrandsByCNCategoryId(String requestId, String userName,
      String categoryId, boolean clearCache) throws Exception;

  /**
   * api to retrieve all the products for suspension based on the filter request.
   *
   * @param summaryFilterRequest
   * @param requestId
   * @param userName
   * @param storeId
   * @param pageable
   * @return
   */
  Page<SuspensionProductResponse> getAllProducts(SummaryFilterRequest summaryFilterRequest, String requestId,
      String userName, String storeId, Pageable pageable) throws Exception;

  /**
   * TakeDown or re-activate product. All l4s under a l3
   * @param storeId
   * @param productSku
   * @param isTakenDown
   * @param productName
   * @throws Exception
   */
  void takeDownOrReactivateProduct(String storeId, String productSku, boolean isTakenDown, String productName,
      ProductL3Response productL3Response) throws Exception;

  /** API to save the suspension history.
   *
   * @param storeId
   * @param productSku
   * @param businessPartnerCode
   * @param reason
   * @param description
   * @param status
   * @param createdBy
   * @param  createdDate
   *
   * */
  void saveSuspensionHistory(String storeId, String productSku, String businessPartnerCode, String reason, String description,
      SuspensionStatus status, String createdBy, Date createdDate);


  /**
   * api to retrieve all the products for suspension based on the filter request.
   *
   * @param summaryFilterRequest
   * @param requestId
   * @param userName
   * @param storeId
   * @param pageable
   * @return
   */
  Page<SuspensionItemResponse> getSuspendedItems(SummaryFilterRequest summaryFilterRequest, String requestId,
      String userName, String storeId, Pageable pageable) throws Exception;

  /**
   * @param storeId
   * @param username
   * @param suspensionProductRequest
   * @throws Exception
   */
  void doSuspensionProductsActions(String storeId, String username, SuspensionProductRequest suspensionProductRequest)
      throws Exception;

  /**
   * Get the suspension history for productSku
   * @param storeId
   * @param productSku
   * @param pageRequest
   * @return
   */
  Page<ProductSuspensionHistory> getSuspensionHistory(String storeId, String productSku, PageRequest pageRequest) throws Exception;


  /**
   * @param productCode
   * @param merchantCode
   * @throws Exception
   */
  List<ProductResponse> getProductsByProductCodeAndMerchantCode(String productCode, String merchantCode)
      throws Exception;

  /**
   * @param storeId
   * @param requestId
   * @param userName
   * @param businessPartnerCode
   * @throws Exception
   */
  CountProductLevel3InactiveResponse countSummaryForInactiveProduct(String storeId, String requestId, String userName,
      String businessPartnerCode) throws Exception;

  /**
   * API to fetch the minimum price from the configuration.
   *
   */
  Integer getMinimumPrice(String storeId);

  /**
   * Compare wholesale rules of product and category for screening and vendor
   *
   *
   * @param storeId
   * @param productCode
   * @param categoryCode
   * @return
   */
  boolean checkCategoryProductWholesaleRules(String storeId, String productCode, String categoryCode) throws Exception;

  /**
   * Update product edit info
   *
   * @param request
   * @param isOnlyExternal
   * @param toTakeDown
   * @param combineContentAndLogisticsPcbUpdate
   * @param combinePreOrderUpdate
   * @param productL3Response
   * @param newImagesAdded
   * @param productCollection
   * @param productL3UpdateRequest
   * @return
   */
  EditProductResponse updateEditInfo(ProductLevel3 request, boolean isOnlyExternal, boolean toTakeDown,
      boolean combineContentAndLogisticsPcbUpdate, ProfileResponse profileResponse, boolean combinePreOrderUpdate,
      ProductL3Response productL3Response,boolean newImagesAdded, ProductCollection productCollection,
      ProductL3UpdateRequest productL3UpdateRequest) throws Exception;

   /**
   * @param storeId
   * @param request
   * @param businessPartnerCode
   * @return
   */
   ItemsPriceStockImagesUpdateResponse updateItemsPriceStockImages(String storeId,
       List<ProductPriceStockAndImagesRequest> request, String businessPartnerCode,
       List<ProductLevel3SummaryDetailsImageRequest> copyToAllVariantImages, boolean productEditable,
       ProductL3Response productL3Response) throws Exception;

  /**
   *
   * @param request
   * @param locationPathAndCommonImages
   * @param productCode
   * @return
   * @throws Exception
   */
  EditedResizeImagesResponse publishEditImagesForResizing(
      List<ProductLevel3SummaryDetailsImageRequest> request, List<LocationPathAndCommonImage> locationPathAndCommonImages, String productCode) throws Exception;

  /**
   * @param storeId
   * @param request
   * @param businessPartnerCode
   * @return
   */
  ItemsPriceStockImagesUpdateResponse editProductItemsPriceStockImages(String storeId,
      UpdateItemsPriceStockImagesRequest request, String businessPartnerCode) throws Exception;

  /**
   * Update product L5 data
   *
   * @param storeId
   * @param businessPartnerCode
   * @param productVariantUpdateRequest
   * @param editResponse
   * @param productLevel3
   * @param productDetailEdit check if Edit is PDP Edit or not
   * @return
   * @throws Exception
   */
  ItemsPriceStockImagesUpdateResponse editProductItemsPriceStockImagesL5(String storeId, String businessPartnerCode,
      ProductVariantUpdateRequest productVariantUpdateRequest, EditProductResponse editResponse, ProductLevel3 productLevel3, boolean productDetailEdit) throws Exception;

  /**
   * Update Fbb at L3 and Populate History
   * @param storeId
   * @param businessPartnerCode
   * @param productVariantUpdateRequest
   * @param itemResponseAndFbbChangePair
   */
  void updateFbbAtL3AndPopulateHistory(String storeId, String businessPartnerCode,
      ProductVariantUpdateRequest productVariantUpdateRequest,
      Pair<ItemsPriceStockImagesUpdateResponse, FbbAndCncDataChangeDto> itemResponseAndFbbChangePair) throws Exception;

  /**
   * Update product L5 data
   *
   * @param storeId
   * @param businessPartnerCode
   * @param copyToAllVariantImages
   * @param productEditable
   * @param productVariantUpdateRequest
   * @param editResponse
   * @param productLevel3
   * @param combinedEditFlowEnabled
   * @return
   * @throws Exception
   */
  ItemsPriceStockImagesUpdateResponse updateVariantsPriceStockImages(String storeId, String businessPartnerCode,
      List<ProductLevel3SummaryDetailsImageRequest> copyToAllVariantImages, boolean productEditable,
      ProductVariantUpdateRequest productVariantUpdateRequest, EditProductResponse editResponse, ProductLevel3 productLevel3,
    boolean combinedEditFlowEnabled) throws Exception;

  /**
   * Validate L5 update request
   *
   * @param productVariantUpdateRequest
   * @param profileResponse
   * @return
   * @throws Exception
   */
  ItemsPriceStockImagesUpdateResponse validateL5UpdateRequest(ProductVariantUpdateRequest productVariantUpdateRequest,
    ProfileResponse profileResponse) throws Exception;

  /**
   * Publish edited image resize event
   *
   * @param productVariantUpdateRequest
   * @param editResponse
   * @param editedResizeAndImagesUpdateStatusResponse
   * @param productCode
   * @return
   */
  EditedResizeAndImagesUpdateStatusResponse publishEditedResizeEventIfApplicable(ProductVariantUpdateRequest productVariantUpdateRequest,
      EditProductResponse editResponse,
      EditedResizeAndImagesUpdateStatusResponse editedResizeAndImagesUpdateStatusResponse, String productCode);

  /**
   * Get takeDown status
   *
   * @param postLive
   * @param itemImagesUpdateStatus
   * @param editedResizeAndImagesUpdateStatusResponse
   * @param productCollection
   * @return
   */
  boolean getTakeDownStatus(boolean postLive, Map<String, Boolean> itemImagesUpdateStatus,
      EditedResizeAndImagesUpdateStatusResponse editedResizeAndImagesUpdateStatusResponse,
      ProductCollection productCollection);

  /**
   * Check product status after edit
   *
   * @param storeId
   * @param productSku
   * @param businessPartnerCode
   * @param successValidationVariantList
   * @param itemImagesUpdateStatus
   * @param productL3Response
   * @param takeDownProduct
   * @return
   * @throws Exception
   */
  PostLiveFlagsDTO productAfterUpdateGoingForReviewIsPostLive(String storeId, String productSku, String businessPartnerCode, List<ProductPriceStockAndImagesRequest> successValidationVariantList,
      Map<String, Boolean> itemImagesUpdateStatus, ProductL3Response productL3Response, boolean takeDownProduct) throws Exception;

  /**
   *
   * @param itemImageUpdateRequestPCB item image update request
   * @param itemSkuItemNameMap itemSku to itemName map
   * @param skuCodeItemSkuMap skuCode to itemSku map
   * @param productImageEditRequestsForPCB product image edit requests for PCB
   * @return List of LocationPathAndCommonImage
   * @throws Exception
   */
  List<LocationPathAndCommonImage> updateItemImages(
    ItemImageUpdateRequestPCB itemImageUpdateRequestPCB, Map<String, String> itemSkuItemNameMap,
    Map<String, String> skuCodeItemSkuMap,
    List<com.gdn.x.productcategorybase.dto.request.ProductImageEditRequest> productImageEditRequestsForPCB)
    throws Exception;

  /**
   *
   *
   * @param businessPartnerCode
   * @param needCorrection
   * @param locationPathAndCommonImages
   * @param productItemImageUpdateRequest
   * @param productSku
   * @param itemSkuItemNameMap
   * @param skuCodeItemSkuMap
   * @param listRequest
   * @return
   * @throws Exception
   */
  List<LocationPathAndCommonImage> updateItemLevelImages(String businessPartnerCode, boolean needCorrection,
      List<LocationPathAndCommonImage> locationPathAndCommonImages,
      ProductItemImageUpdateRequest productItemImageUpdateRequest, String productSku,
      Map<String, String> itemSkuItemNameMap, Map<String, String> skuCodeItemSkuMap,
      List<com.gdn.x.productcategorybase.dto.request.ProductImageEditRequest> listRequest) throws Exception;

  /**
   *
   * @param storeId
   * @param filterRequest
   * @param pageRequest
   * @return
   * @throws Exception
   */
  Page<ProductLevel3SummaryDetails> findSummaryDetailsByFilter(String storeId, ProductLevel3SummaryFilterDetails filterRequest,
      Pageable pageRequest) throws Exception;

  /**
   * Get pickupPointCodes of L4s under a L3 in a paginated form
   *
   * @param productSku
   * @param page
   * @param size
   * @param needCorrection
   * @param storeId
   * @param businessPartnerCode
   * @param fbbActivated
   * @return
   * @throws Exception
   */
  Page<PickupPointCodeResponse> getPickupPointCodes(String productSku, int page, int size, boolean needCorrection,
      String storeId, String businessPartnerCode, boolean fbbActivated) throws Exception;

  /**
   * Get all distinct pickupPointsCodes for all the L4s under L3
   * @param productSku
   * @return
   * @throws Exception
   */
  UniquePickupPointCodeResponse getUniquePickupPointCodes(String productSku) throws Exception;

  /**
   * L5 need revision update
   *
   * @param storeId
   * @param request
   * @param businessPartnerCode
   * @return
   * @throws Exception
   */
  Pair<ItemsPriceStockImagesUpdateResponse, FbbAndCncDataChangeDto> saveItemsPriceStockImagesNeedCorrectionL5(
      String storeId, ProductVariantUpdateRequest request, String businessPartnerCode, EditProductResponse editProductResponse) throws Exception;

  /**
   * API to bulk update pickup point codes
   * @param request
   * @return
   * @throws Exception
   */
  PickupPointUpdateResponse updatePickupPointCodes(PickupPointUpdateRequest request) throws Exception;

  /**
   * API to update the logistics information
   *
   * @param product
   * @param isOnlyExternal
   * @param productLevel3DetailResponse
   * @param combineContentAndLogisticsPcbUpdate
   * @param combinePreOrderUpdate
   * @param migrateProduct
   * @return
   * @throws Exception
   */
  ApiErrorCode updateLogistics(ProductLevel3UpdateRequest product, boolean isOnlyExternal,
      ProductLevel3DetailResponse productLevel3DetailResponse, boolean combineContentAndLogisticsPcbUpdate,
      boolean combinePreOrderUpdate, boolean migrateProduct) throws Exception;

  /**
   * Update need revision for need revision product
   *
   * @param product
   * @throws Exception
   */
  ApiErrorCode updateLogisticsForNeedRevision(ProductLevel3UpdateRequest product) throws Exception;

  /**
   * Fetch product sku summary by request filter
   *
   * @param storeId
   * @param requestId
   * @param businessPartnerCode
   * @param page
   * @param size
   * @param productL3SummaryRequest
   * @return
   */
  Page<ProductL3SummaryResponse> getProductL3Summary(String storeId, String requestId,
      String businessPartnerCode, int page, int size,
      ProductL3SummaryRequest productL3SummaryRequest) throws Exception;

  /**
   * @param productSku
   * @param page
   * @param size
   * @return
   * @throws Exception
   */
  Page<ProductItemNameResponse> getItemSummaryResponseByProductSku(String productSku, int page, int size)
      throws Exception;

  /**
   * API to fetch the L3 detail by product sku.
   * @param productSku
   * @param isNeedCorrection
   * @param fullFetch
   * @param pickupPointDeleteRequests
   * @param addingPickupPoints
   * @return
   * @throws Exception
   */
  ProductLevel3DetailResponse getL3DetailByProductSku(String productSku, boolean isNeedCorrection, boolean fullFetch, List<PickupPointDeleteRequest> pickupPointDeleteRequests, boolean addingPickupPoints) throws Exception;

    /**
     * Generate Product Level 3 Detail Response
     *
     * @param productData saved product data
     * @param categories categories of the product
     * @param productLevel3Logistics logistics of the product
     * @param profileResponse business partner profile
     * @return ProductLevel3DetailResponse
     * @throws Exception
     */
  ProductLevel3DetailResponse generateProductLevel3Detail(ProductL3Response productData,
    List<CategoryResponse> categories, List<ProductLevel3Logistics> productLevel3Logistics,
    ProfileResponse profileResponse) throws Exception;

  /**
   * Check if new varaint creation attribute is added
   *
   * @param product
   * @param productItemAttributeValueMap
   * @param newProductAttributes
   * @param existingAttributeCodeToValuesMap
   * @param attributeCodeToValuesRequestMap
   * @param deletedAttributeCodes
   * @param request
   * @throws Exception
   */
  void checkIfValuesAreUpdatedToExistingAttribute(Product product,
      Map<String, ProductItemAttributeValueRequest> productItemAttributeValueMap,
      Map<String, ProductLevel3Attribute> newProductAttributes,
      Map<String, List<String>> existingAttributeCodeToValuesMap,
      Map<String, List<String>> attributeCodeToValuesRequestMap, List<String> deletedAttributeCodes,
      ProductLevel3 request) throws Exception;

  /**
   * API to archive a list of products
   * @param productSkuList
   * @param doArchive
   * @return
   * @throws Exception
   */
  ItemBulkArchiveResponse toggleArchiveProducts(List<String> productSkuList, boolean doArchive) throws Exception;

  /**
   * Api to fetch suspension history by product sku list
   *
   * @param storeId
   * @param productSkuList
   * @return
   */
  List<ProductSuspensionHistoryResponse> getProductSuspensionHistoryByProductSkus(String storeId,
      List<String> productSkuList);

  /**
   * Api to productCodes based on productIds
   *
   * @param storeId
   * @param productIds
   * @return
   */
  Map<String, String> getProductIdProductCodeMap(String storeId, List<String> productIds);

  /**
   * re-activate product on need correction. All l4s under a l3
   *
   * @param storeId
   * @param productSku
   * @param profileResponse
   * @param categoryResponseList
   * @throws Exception
   */
  void activateProductOnNeedCorrection(String storeId, String productSku,
    ProfileResponse profileResponse, List<ProductCategoryResponse> categoryResponseList) throws Exception;

  /**
   * re-activate product activation for flow 2 products
   *
   * @param storeId
   * @param productSkuList
   * @throws Exception
   */
  void retrySkipReviewProductActivation(String storeId, List<String> productSkuList);

  /**
   *
   * @param productLevel3ViewConfigRequest
   * @param productSku
   * @throws Exception
   */
  void updateProductItemViewConfig(ProductLevel3ViewConfigStockRequest productLevel3ViewConfigRequest, String productSku)
      throws Exception;

  /**
   *
   * @param request
   * @param productSku
   * @return
   */
  EditProductResponse updateProductLevel3Info(UpdateProductLevel3InfoRequest request, String productSku)
      throws Exception;

  /**
   * Fetch cogs value by material code
   *
   * @param materialCode
   * @return
   */
  CogsValueResponse fetchCogsValueByMaterialCode(String materialCode);

  /**
   * API to update images
   * @param request
   * @return
   * @throws Exception
   */
  ItemsPriceStockImagesUpdateResponse updateImages(String storeId, ProductImageEditRequest request) throws Exception;

  /**
   * get item cogs value
   * @param businessPartnerCode
   * @param itemCodes
   * @return
   * @throws Exception
   */
  Map<String, ProductItemsCogs> getProductItemCogsValues(String businessPartnerCode, List<String> itemCodes)
      throws Exception;

  /**
   *
   * @param l3UpdateRequest
   * @param variantUpdateRequest
   * @return ItemsPriceStockImagesUpdateResponse
   * @throws Exception
   */
  ItemsPriceStockImagesUpdateResponse updateProductItemImages(
    ProductL3UpdateRequest l3UpdateRequest, ProductVariantUpdateRequest variantUpdateRequest)
    throws Exception;


  List<UpsertOfflineItem> upsertL5StockInInventory(String merchantCode, List<UpsertOfflineItem> updateOfflineItems,
      List<UpsertOfflineItemFailedResponse> failedOfflineItemResponses, ProfileResponse profileResponse)
      throws Exception;

  /**
   * Update solr
   * @param productCollection
   */
  public void updateSolrProductCollectionDocument(ProductCollection productCollection);

  /**
   * Update product collection
   * @param productCollection
   * @return
   */
  public ProductCollection updateProductCollection(ProductCollection productCollection);

  /**
   * Process and validate seller penalty check on stocks
   * @param productVariantUpdateRequest Request Model
   * @param profileResponse Profile Response of business partner
   * @param sellerPenaltyEnabledPhase2 Switch for validation
   */
  void processSellerPenaltyChecks(ProductVariantUpdateRequest productVariantUpdateRequest,
    ProfileResponse profileResponse, boolean sellerPenaltyEnabledPhase2);
}
