package com.gdn.partners.pbp.outbound.xProduct;

import java.util.List;
import java.util.Map;
import java.util.Set;

import com.gda.mta.product.dto.NeedCorrectionProductActivationRequest;
import com.gdn.partners.pbp.outbound.xProduct.feign.ProductBasicMasterFieldsRequest;
import com.gdn.x.product.rest.web.model.request.CogsUpdateListRequest;
import com.gdn.x.product.rest.web.model.request.ProductAndL5MigrationRequest;
import com.gdn.x.product.rest.web.model.response.ItemPickupPointBasicResponse;
import com.gdn.x.product.rest.web.model.response.ItemPickupPointL5Response;
import com.gdn.x.product.rest.web.model.response.ProductCenterDetailResponse;
import com.gdn.x.product.rest.web.model.response.SimpleBooleanResponse;
import com.gdn.x.product.rest.web.model.response.CogsResponse;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;

import com.gda.mta.product.dto.EditFlagChangesDTO;
import com.gda.mta.product.dto.ProductDetailEditDTO;
import com.gda.mta.product.dto.ProductVariantUpdateRequest;
import com.gdn.common.exception.ApplicationException;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.x.product.enums.ProductType;
import com.gdn.x.product.rest.web.model.ActivateNeedRevisionResponse;
import com.gdn.x.product.rest.web.model.CombinedEditItemResponse;
import com.gdn.x.product.rest.web.model.EditItemResponse;
import com.gdn.x.product.rest.web.model.L3VersionResponse;
import com.gdn.x.product.rest.web.model.request.AddDeleteVariantRequest;
import com.gdn.x.product.rest.web.model.request.AddDeleteVariantRetryRequest;
import com.gdn.x.product.rest.web.model.request.CreateFbbPickupPointRequest;
import com.gdn.x.product.rest.web.model.request.DeleteItemPickupPointRequest;
import com.gdn.x.product.rest.web.model.request.DeleteOfflineItemRequest;
import com.gdn.x.product.rest.web.model.request.ItemPickupPointDeleteRequest;
import com.gdn.x.product.rest.web.model.request.ItemPickupPointListingRequest;
import com.gdn.x.product.rest.web.model.request.ItemPickupPointQuickEditRequest;
import com.gdn.x.product.rest.web.model.request.ItemPickupPointRequest;
import com.gdn.x.product.rest.web.model.request.ItemPickupPointSummaryRequest;
import com.gdn.x.product.rest.web.model.request.ItemRequest;
import com.gdn.x.product.rest.web.model.request.ItemRequestV2;
import com.gdn.x.product.rest.web.model.request.ItemViewConfigAndItemSkuRequest;
import com.gdn.x.product.rest.web.model.request.ItemsSummaryDetailRequest;
import com.gdn.x.product.rest.web.model.request.PickupPointUpdateRequest;
import com.gdn.x.product.rest.web.model.request.ProductAndItemActivationRequest;
import com.gdn.x.product.rest.web.model.request.ProductDetailPageEditRequest;
import com.gdn.x.product.rest.web.model.request.ProductEditRequest;
import com.gdn.x.product.rest.web.model.request.ProductRequest;
import com.gdn.x.product.rest.web.model.request.ProductSkuAndProductCodeRequest;
import com.gdn.x.product.rest.web.model.request.ProductSkuSummaryRequest;
import com.gdn.x.product.rest.web.model.request.ProductSummaryRequest;
import com.gdn.x.product.rest.web.model.request.ProductSummaryRequestV2;
import com.gdn.x.product.rest.web.model.request.QuickEditUpdateRequest;
import com.gdn.x.product.rest.web.model.request.SimpleListStringRequest;
import com.gdn.x.product.rest.web.model.request.SimpleSetStringRequest;
import com.gdn.x.product.rest.web.model.response.AddProductAndItemsResponse;
import com.gdn.x.product.rest.web.model.response.BasicProductResponse;
import com.gdn.x.product.rest.web.model.response.BusinessPartnerPickupPointResponse;
import com.gdn.x.product.rest.web.model.response.BusinessPartnerResponse;
import com.gdn.x.product.rest.web.model.response.CreateFbbPickupPointResponse;
import com.gdn.x.product.rest.web.model.response.DeleteItemPickupPointResponse;
import com.gdn.x.product.rest.web.model.response.DeleteOfflineItemResponse;
import com.gdn.x.product.rest.web.model.response.DuplicateProductDetailsResponse;
import com.gdn.x.product.rest.web.model.response.ItemBasicDetailV2Response;
import com.gdn.x.product.rest.web.model.response.ItemImagesListResponse;
import com.gdn.x.product.rest.web.model.response.ItemLevel5Response;
import com.gdn.x.product.rest.web.model.response.ItemPickupPointCodeResponse;
import com.gdn.x.product.rest.web.model.response.ItemPickupPointListingResponse;
import com.gdn.x.product.rest.web.model.response.ItemResponse;
import com.gdn.x.product.rest.web.model.response.ItemResponseV2;
import com.gdn.x.product.rest.web.model.response.ItemSkuPickupPointCodeResponse;
import com.gdn.x.product.rest.web.model.response.ItemSummaryDetailResponse;
import com.gdn.x.product.rest.web.model.response.ItemSummaryListResponse;
import com.gdn.x.product.rest.web.model.response.MinMaxItemPriceResponse;
import com.gdn.x.product.rest.web.model.response.PickupPointDetailResponse;
import com.gdn.x.product.rest.web.model.response.PrdProductResponse;
import com.gdn.x.product.rest.web.model.response.ProductAndItemsResponse;
import com.gdn.x.product.rest.web.model.response.ProductBasicResponse;
import com.gdn.x.product.rest.web.model.response.ProductCountResponse;
import com.gdn.x.product.rest.web.model.response.ProductL3Response;
import com.gdn.x.product.rest.web.model.response.ProductL3SummaryResponse;
import com.gdn.x.product.rest.web.model.response.ProductL5DetailResponse;
import com.gdn.x.product.rest.web.model.response.ProductResponse;
import com.gdn.x.product.rest.web.model.response.ProductSkuSummaryResponse;
import com.gdn.x.product.rest.web.model.response.ProductSummaryResponseV2;
import com.gdn.x.product.rest.web.model.response.ProductTypeResponse;
import com.gdn.x.product.rest.web.model.response.SharedProductBundleRecipeResponse;
import com.gdn.x.product.rest.web.model.response.SimpleLongResponse;
import com.gdn.x.product.rest.web.model.response.SimpleMapStringResponse;

public interface XProductOutbound {

  ProductResponse updateProduct(boolean isOnlyExternal, ProductRequest productRequest) throws Exception;

  ItemResponse updateItem(boolean isOnlyExternal, boolean isProductTypeChanged, boolean isPreOrderchanged,
      ItemRequest itemRequest) throws Exception;

  void updateItemViewConfigAndForceReview(boolean forceReview,
      List<ItemViewConfigAndItemSkuRequest> itemViewConfigAndItemSkuListRequest, boolean isArchived);

  /**
   * update item view config
   * @param forceReview
   * @param itemViewConfigAndItemSkuListRequest
   * @param isArchived
   * @param scheduleRemoval
   */
  void updateItemViewConfigAndForceReview(boolean forceReview,
      List<ItemViewConfigAndItemSkuRequest> itemViewConfigAndItemSkuListRequest, boolean isArchived,
      boolean scheduleRemoval);

  GdnRestSingleResponse<ProductAndItemsResponse> getProductAndItems(boolean showDeleted, String productSku,
      boolean includeForceReview);

  GdnRestSingleResponse<ProductAndItemsResponse> getProductAndItemsWithProductData(boolean showDeleted, String productSku,
      boolean includeForceReview, boolean isMigrateAndSyncProduct);

  List<DeleteOfflineItemResponse> bulkDeleteOfflineItem(String merchantCode,
      List<DeleteOfflineItemRequest> deleteOfflineItemRequests) throws Exception;

  void updateMigratedProductCode(String productSku, String newProductCode, boolean rollback) throws Exception;

  List<BusinessPartnerResponse> getBusinessPartnerDetails(List<String> businessPartnerCodes) throws Exception;

  Page<ProductSkuSummaryResponse> getProductSkuSummary(String businessPartnerCode,
      ProductSkuSummaryRequest productL3SummaryRequest, int page, int size);

  Long updatePickupPointCodes(PickupPointUpdateRequest pickupPointUpdateRequest) throws Exception;

  Set<String> getPickupPointCodesByProductSku(String productSku) throws Exception;


  /**
   * @param filter
   * @param pageRequest
   * @return
   * @throws Exception
   */
  Page<ItemSummaryDetailResponse> findSummaryDetailsByFilter(ItemsSummaryDetailRequest filter, Pageable pageRequest)
      throws Exception;

  /**
   * API to fetch the L3 details by product sku.
   *
   * @param productSku
   * @return
   * @throws Exception
   */
  GdnRestSingleResponse<ProductL3Response> getProductDetailsByProductSku(String productSku) throws Exception;

  /**
   * API to archive product at L3 level.
   *
   * @param productSku
   * @return
   * @throws Exception
   */
  void toggleArchiveProduct(String productSku, boolean doArchive) throws Exception;

  void updateItemListing(String productSku, ProductType productType, List<QuickEditUpdateRequest> quickEditRequest);

  /**
   * @param productSkus
   * @param pickupPointCodes
   * @param promoTypes
   * @param fetchB2bData
   * @param fetchViewConfigByChannel
   *
   * @return
   */
  List<ItemLevel5Response> getL5ItemListing(Set<String> productSkus, List<String> pickupPointCodes,
      List<String> promoTypes, boolean fetchB2bData, String fetchViewConfigByChannel);

  void updateOff2OnActiveFlagByProductSku(Map<String, Boolean> off2OnFLagByProductSkuMap);

  /**
   * update category when auto category change occurs
   * @param productSku
   * @param productCode
   * @param updateCategory
   */
  L3VersionResponse generateProductScoreByProductSkuOrProductCode(String productSku, String productCode, boolean updateCategory);

  /**
   * validate duplicate product by sellerSku
   *
   * @param sellerSku
   * @param merchantCode
   * @return
   */
  DuplicateProductDetailsResponse validateDuplicateProductBySellerSku(String sellerSku,
      String merchantCode);

  /**
   * get prd_product by productSku or productCode
   *
   * @param prdProductRequest
   * @return
   */
  List<PrdProductResponse> getPrdProductDetailByProductSkuOrProductCode(
      ProductSkuAndProductCodeRequest prdProductRequest);

  /**
   * get pickup points for items
   * @param productSku
   * @param page
   * @param size
   * @param fbbActivated
   * @return
   */
  Page<ItemPickupPointCodeResponse> getItemPickupPointCodeResponse(String productSku, int page, int size,
      boolean fbbActivated);

  /**
   * API to update the content change of the product
   *
   * @param productSku
   * @param contentChange
   * @param publishItems
   * @return
   * @throws Exception
   */
  void updateContentChange(String productSku, boolean contentChange, boolean publishItems) throws Exception;

  /**
   * API to get product counts by type
   *
   * @param businessPartnerCode
   * @param type
   * @return
   * @throws Exception
   */
  ProductCountResponse getProductCountByType(String businessPartnerCode, String type) throws Exception;

  /**
   * API to fetch the item detail by item sku
   *
   * @param itemSku
   * @param convertPreOrderDetails
   * @return
   * @throws Exception
   */
  GdnRestSingleResponse<ProductAndItemsResponse> getProductAndSingleItemByItemSku(String itemSku,
      boolean convertPreOrderDetails);

  /**
   *
   * @param productEditRequest
   * @param updateCategory
   */
  void updateEditedProduct(ProductEditRequest productEditRequest, boolean updateCategory);

  /**
   * Get list of item images by itemSkus
   *
   * @param itemSkusSet
   * @return
   */
  List<ItemImagesListResponse> getListOfImagesByItemSkus(SimpleSetStringRequest itemSkusSet);

  /**
   * API to update product on need correction activation
   *
   * @param request
   * @return
   * @throws Exception
   */
  ActivateNeedRevisionResponse activateOnNeedCorrection(NeedCorrectionProductActivationRequest request) throws Exception;

  /**
   * API to get type based on code
   *
   * @param productCode
   * @return
   * @throws Exception
   */
  ProductTypeResponse getProductTypeByProductCode(String productCode);

  /**
   *
   * @param itemSkus
   * @param wholeSalePriceActivated
   */
  void updateWholeSaleActivationFlag(List<String> itemSkus, boolean wholeSalePriceActivated);

  /**
   * Fetch pickup point details by list of pickup point codes
   *
   * @param pickupPointCodes
   * @return
   */
  List<BusinessPartnerPickupPointResponse> getPickupPointDetailsByListOfPickupPointCodes(
    List<String> pickupPointCodes);

  /**
   * API to get ItemPickupPointCodes By ItemSkus
   *
   * @param itemSkusList
   * @return
   */
  GdnRestListResponse<ItemSkuPickupPointCodeResponse> getItemPickupPointCodeByItemSkus(
      SimpleListStringRequest itemSkusList);

  /**
   *
   * @param itemPickupPointRequest
   * @param fetchViewConfigByChannel
   * @return
   */
  List<ItemSummaryListResponse> getItemPickupPointSummary(List<ItemPickupPointRequest> itemPickupPointRequest,
      String fetchViewConfigByChannel);

  /**
   *
   *  @param productSku
   * @param productType
   * @param itemPickupPointListingUpdateRequestVos*/
  void updateItemPickupPointListing(String productSku, ProductType productType, List<ItemPickupPointQuickEditRequest> itemPickupPointListingUpdateRequestVos);

  /**
   * @param productSku
   */
  void updateMasterDataFieldsInProduct(String productSku);

  /**
   * Create l3 and l4 in x-product
   * @param storeId
   * @param channelId
   * @param clientId
   * @param username
   * @param productAndItemActivationRequest
   * @return
   */
  AddProductAndItemsResponse addProductAndItems(String storeId, String channelId, String clientId, String requestId,
      String username, ProductAndItemActivationRequest productAndItemActivationRequest) throws ApplicationException;

  /**
   * Get l5 listing
   * @param storeId
   * @param channelId
   * @param clientId
   * @param requestId
   * @param username
   * @param page
   * @param size
   * @param itemPickupPointListingRequest
   * @return
   */
  Page<ItemPickupPointListingResponse> getItemPickupPointList(String storeId, String channelId, String clientId, String requestId,
      String username, int page, int size, ItemPickupPointListingRequest itemPickupPointListingRequest);

  /**
   * @param itemSkus
   * @param includeMarkForDelete
   * @return
   */
  GdnRestSingleResponse<SimpleMapStringResponse> getItemNameByItemSkus(SimpleListStringRequest itemSkus,
      boolean includeMarkForDelete) throws ApplicationException;

  /**
   * Get list of Pickup point names in detail response by pickup point codes
   *
   * @param pickupPointCodes
   * @param fbbActivated
   * @return
   */
  List<PickupPointDetailResponse> getPickupPointDetailResponse(List<String> pickupPointCodes, boolean fbbActivated);

  /**
   * Update product item pickup points
   *
   * @param storeId
   * @param productSku
   * @param productType
   * @param quickEditUpdateRequests
   * @param addPickupPointRequests
   * @param deletePickupPointRequests
   * @param online
   * @param isOnlineFlagChanged
   * @param cnc
   * @param isCNCFlagChangedAtL3Level
   * @param fbbActivated
   * @param isFbbFlagChangedAtL3Level
   * @param addDeleteVariantRequest
   * @param productVariantUpdateRequest
   * @param editFlagChangesDTO
   */
  EditItemResponse updateItemPickupPoints(String storeId, String productSku, ProductType productType,
      List<ItemPickupPointQuickEditRequest> quickEditUpdateRequests,
      List<ItemPickupPointQuickEditRequest> addPickupPointRequests,
      List<ItemPickupPointDeleteRequest> deletePickupPointRequests, Boolean online, boolean isOnlineFlagChanged,
      Boolean cnc, boolean isCNCFlagChangedAtL3Level, Boolean fbbActivated, boolean isFbbFlagChangedAtL3Level,
      AddDeleteVariantRequest addDeleteVariantRequest, ProductVariantUpdateRequest productVariantUpdateRequest,
      EditFlagChangesDTO editFlagChangesDTO);

  /**
   * Fetch L5 details by request
   *
   * @param page
   * @param size
   * @param itemPickupPointSummaryRequest
   * @return
   */
  Page<ItemResponseV2> getItemPickupPointSummary(int page, int size,
    ItemPickupPointSummaryRequest itemPickupPointSummaryRequest);

  /**
   *
   * get L3 summary details
   * @param page
   * @param size
   * @param productSummaryRequest
   * @return
   * @throws ApplicationException
   */
  GdnRestListResponse<ProductL3SummaryResponse> filterSummaryL3(int page,
      int size, ProductSummaryRequest productSummaryRequest) throws ApplicationException;

  /**
   * @param page
   * @param size
   * @param productSummaryRequestV2
   * @return
   * @throws ApplicationException
   */
  GdnRestListResponse<ProductSummaryResponseV2> getProductSummary(int page, int size,
    ProductSummaryRequestV2 productSummaryRequestV2) throws ApplicationException;

  /**
   *
   * @param itemRequestV2
   * @return
   * @throws ApplicationException
   */
  List<ItemResponseV2> getItemPickupPointsByItemSku(ItemRequestV2 itemRequestV2) throws ApplicationException;

  /**
   * update cnc activation flags by item-skus
   * @param itemSkus
   */
  void updateCncActivationFlag(List<String> itemSkus);

  /**
   * delete active l5's by pp code
   * @param deleteItemPickupPointRequest
   */
  List<DeleteItemPickupPointResponse> deleteActiveItemPickupPointByPickupPointCode(DeleteItemPickupPointRequest deleteItemPickupPointRequest);

  /**
   * create fbb pickup point by item sku and pickup point code
   *
   * @param createFbbPickupPointRequest
   * @return
   */
  CreateFbbPickupPointResponse createFbbPickupPoint(CreateFbbPickupPointRequest createFbbPickupPointRequest);

  /**
   * get min and max offer price
   * @param productCode
   * @return
   */
  MinMaxItemPriceResponse getMinAndMaxOfferPrice(String productCode);

  /**
   *
   * @param itemPickupPointRequest request with itemSku and pickup point code
   * @return ProductL5DetailResponse
   */
  List<ProductL5DetailResponse> findProductAndItemByItemSkuAndPickupPointCode(List<ItemPickupPointRequest> itemPickupPointRequest);

  /**
   * getting basic item details by product sku
   *
   * @param productSku
   * @return
   * @throws ApplicationException
   */
  List<ItemBasicDetailV2Response> findItemBasicDetailsByProductSku(String productSku);

  /**
   * get basic product info
   * @param productSku
   * @return
   */
  BasicProductResponse getBasicProductInfo(String productSku);

  /**
   *
   * @param productSku
   * @return
   */
  BasicProductResponse getBasicProductInfoV2(String productSku);

  /**
   * Update Product, Item and itemPickupPoint for PDP Edit Request
   *
   * @param productSku not null
   * @param updateCategory boolean
   * @return productDetailPageEditRequest Edit Request
   * @throws ApplicationException
   */
  CombinedEditItemResponse updateEditedProductAndItemPickupPoint(String productSku,
    boolean updateCategory, ProductDetailPageEditRequest productDetailPageEditRequest) throws Exception;

  /**
   * Update product item pickup points for combined edit
   *
   * @param storeId
   * @param productSku
   * @param productType
   * @param quickEditUpdateRequests
   * @param addPickupPointRequests
   * @param deletePickupPointRequests
   * @param online
   * @param isOnlineFlagChanged
   * @param cnc
   * @param isCNCFlagChangedAtL3Level
   * @param fbbActivated
   * @param isFbbFlagChangedAtL3Level
   * @param addDeleteVariantRequest
   * @param productVariantUpdateRequest
   * @param editFlagChangesDTO
   * @param productDetailEditDTO
   */
  void updateItemPickupPointsForCombinedEdit(String storeId, String productSku,
      ProductType productType, List<ItemPickupPointQuickEditRequest> quickEditUpdateRequests,
      List<ItemPickupPointQuickEditRequest> addPickupPointRequests,
      List<ItemPickupPointDeleteRequest> deletePickupPointRequests, Boolean online, boolean isOnlineFlagChanged,
      Boolean cnc, boolean isCNCFlagChangedAtL3Level, Boolean fbbActivated, boolean isFbbFlagChangedAtL3Level,
      AddDeleteVariantRequest addDeleteVariantRequest, ProductVariantUpdateRequest productVariantUpdateRequest,
      EditFlagChangesDTO editFlagChangesDTO, ProductDetailEditDTO productDetailEditDTO);

  /**
   *
   * @param productSkus
   * @return
   */
  List<ProductBasicResponse> getProductBasicDetails(List<String> productSkus);

  /**
   *
   * @param itemSkus
   * @param fetchBundleRecipe
   * @return
   */
  List<ItemBasicDetailV2Response> getItemBasicDetailV2Response(List<String> itemSkus,
      boolean fetchBundleRecipe);

  /**
   * Get item basic details by itemSkus
   *
   * @param inAllProducts
   * @param itemSkuList
   * @return
   */
  List<ItemBasicDetailV2Response> getItemBasicDetailsByItemSkus(boolean inAllProducts,
      SimpleListStringRequest itemSkuList);

  /**
   * Get Secondary Filter Counts
   * @param type
   * @param businessPartnerCode
   * @return
   */
  ProductCountResponse getSecondaryCounts(String type, String businessPartnerCode);

  /**
   * get l5 count
   * @param itemSku
   * @return
   */
  SimpleLongResponse getL5CountByItemSku(String itemSku);

  /**
   * Reconcile product variants
   *
   * @param addDeleteVariantRetryRequest
   * @param productSku
   * @return
   */
  List<ItemPickupPointCodeResponse> reconcileProductVariants(AddDeleteVariantRetryRequest addDeleteVariantRetryRequest,
      String productSku);

  /**
   * get shared product bundle details
   * @param itemCodes
   * @return
   */
  List<SharedProductBundleRecipeResponse> getSharedProductBundleRecipeDetails(Set<String> itemCodes);

  /**
   * get product sku detail response
   *
   * @param productSku String
   * @return ProductCenterDetailResponse
   */
  ProductCenterDetailResponse getProductSkuDetailResponse(String productSku);

  /**
   * get basic L5 details for list of Item Sku and pp codes
   * @param itemPickupPointRequests list of item sku and pp codes
   * @return ItemPickupPointBasicResponse
   */
  List<ItemPickupPointBasicResponse> fetchBasicDetailsByItemSkuAndPickupPointCodeList(
    List<ItemPickupPointRequest> itemPickupPointRequests);


  /**
   * Migrate Product And L5 Detail By Product Sku
   *
   * @param productAndL5MigrationRequest product And L5 Migration Request
   * @param storeId                      not null
   * @param username updated by
   */
  void migrateProductAndL5DetailByProductSku(
    ProductAndL5MigrationRequest productAndL5MigrationRequest, String storeId, String username);

  /**
   * fetch L5 Responses By ItemSkus
   *
   * @param itemSkus list of itemSkus
   * @return List of ItemPickupPointL5Response
   */
  GdnRestListResponse<ItemPickupPointL5Response> fetchL5ResponsesByItemSkus(List<String> itemSkus, int page,
    int size);

  /**
   * Update Product Master Data Info in x-product
   *
   * @param userName                        updated by
   * @param productBasicMasterFieldsRequest update request for product master data
   */
  void updateProductMasterDataInfo(String userName,
    ProductBasicMasterFieldsRequest productBasicMasterFieldsRequest);

  /**
    * Get CNC at L5 by product SKU
   * @param productSku product SKU
   * @return
   */
  SimpleBooleanResponse getCncAtL5ByProductSku(String productSku);

  /**
   * Update COGS value for product
   * @param productSku product SKU
   * @param request COGS update request
   * @throws Exception
   */
  void updateCogsValue(String productSku, CogsUpdateListRequest request) throws Exception;

  /**
   * Get COGS data for product
   * @param productSku product SKU
   * @param page page number
   * @param size page size
   * @return list of COGS responses
   * @throws Exception
   */
  List<CogsResponse> getCogsData(String productSku, int page, int size) throws Exception;
}
