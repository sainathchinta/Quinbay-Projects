package com.gdn.x.productcategorybase.service;

import java.util.List;
import java.util.Map;
import java.util.Set;

import com.gdn.x.productcategorybase.domain.event.model.CommonImageBackfillingEventModel;
import com.gdn.x.productcategorybase.domain.event.model.CompressedVideoUpdateEventModel;
import com.gdn.x.productcategorybase.domain.event.model.ProductSuitabilityEventModel;
import com.gdn.x.productcategorybase.dto.ProductAndItemLevelUpdatesDTO;
import com.gdn.x.productcategorybase.dto.ProductBrandUpdateDTO;
import com.gdn.x.productcategorybase.dto.ProductBrandUpdateResponseDTO;
import com.gdn.x.productcategorybase.dto.request.EditProductDetailRequest;
import com.gdn.x.productcategorybase.dto.request.NeedRevisionConfigRequest;
import com.gdn.x.productcategorybase.dto.request.ProductBrandDataUpdateRequest;
import com.gdn.x.productcategorybase.dto.request.ProductCodesRequest;
import com.gdn.x.productcategorybase.dto.request.ProductMasterDataUpdateRequest;
import com.gdn.x.productcategorybase.dto.response.EditProductItemAndImageResponse;
import com.gdn.x.productcategorybase.dto.response.NewlySavedItemResponse;
import com.gdn.x.productcategorybase.dto.response.ProductBrandUpdateResponse;
import com.gdn.x.productcategorybase.dto.response.ProductResponse;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;

import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.inventory.dto.WarehouseMasterSKUEvent;
import com.gdn.x.productcategorybase.dto.CategorySummaryResponse;
import com.gdn.x.productcategorybase.dto.LocationPathAndCommonImage;
import com.gdn.x.productcategorybase.dto.SimpleMasterProductUpdateDTO;
import com.gdn.x.productcategorybase.dto.SimpleMasterProductUpdateResponseDTO;
import com.gdn.x.productcategorybase.dto.request.BatchVatUpdateRequest;
import com.gdn.x.productcategorybase.dto.request.ProductImageEditRequest;
import com.gdn.x.productcategorybase.dto.request.ProductItemImageUpdateRequest;
import com.gdn.x.productcategorybase.dto.response.AttributeHistoryResponse;
import com.gdn.x.productcategorybase.dto.response.BatchVatUpdateResponse;
import com.gdn.x.productcategorybase.dto.response.ImageResponse;
import com.gdn.x.productcategorybase.dto.response.ProductAndItemImageRequest;
import com.gdn.x.productcategorybase.dto.response.ProductItemCompleteResponse;
import com.gdn.x.productcategorybase.dto.response.ProductMasterDataItemResponse;
import com.gdn.x.productcategorybase.dto.response.ProductMasterDataResponse;
import com.gdn.x.productcategorybase.dto.response.SimpleItemDetailResponse;
import com.gdn.x.productcategorybase.entity.Product;
import com.gdn.x.productcategorybase.entity.ProductAttribute;
import com.gdn.x.productcategorybase.entity.ProductItem;

public interface ProductServiceWrapper {

  /**
   * Update master data product data
   * @param storeId
   * @param simpleMasterProductUpdateDTO
   * @return
   */
  SimpleMasterProductUpdateResponseDTO updateMasterProductData(
      String storeId, SimpleMasterProductUpdateDTO simpleMasterProductUpdateDTO);

  /**
   * Update product and item images
   *  @param productAndItemImageRequest
   * @param storeId
   */
  boolean updateProductAndItemImagesByProductCode(ProductAndItemImageRequest productAndItemImageRequest, String storeId, boolean setDgLevel);

  /**
   *
   * @param storeId
   * @param productCodes
   */
  void republishProductByProductCodesToAgp(String storeId, List<String> productCodes, boolean split);

  /**
   *  Get list of L2s mapped to any L1 using productCode
   *
   * @param storeId
   * @param productCode
   * @param page
   * @param size
   * @return
   */
  Page<SimpleItemDetailResponse> getItemListByProductCode(String storeId, String productCode, int page, int size) throws Exception;

  /**
   * Update product category by storeId and productCode
   *
   * @param storeId
   * @param productCode
   * @param categoryCode
   * @param updateSalesCategory
   * @param b2bSeller
   * @return
   */
  CategorySummaryResponse updateProductCategoryByStoreIdAndProductCode(String storeId, String productCode,
      String categoryCode, boolean updateSalesCategory, boolean b2bSeller) throws Exception;

  /**
   *
   * Update product dimensions by storeId and productCode
   * @param warehouseMasterSKUEvent
   * @return
   */
  boolean updateProductDimensions(WarehouseMasterSKUEvent warehouseMasterSKUEvent)
      throws Exception;

  /**
   * Migrate FinalImage From Gfs To Gcs
   *
   * @param productCodeList
   * @param storeId
   * @throws Exception
   */
  void migrateFinalImageFromGfsToGcs(List<String> productCodeList, String storeId) throws Exception;

  /**
   * Fetch item details with complete product details
   *
   * @param storeId
   * @param itemCode
   */
  ProductItemCompleteResponse getItemResponseByItemCode(String storeId, String itemCode);

  /**
   *
   * @param storeId
   * @param productCode
   * @return
   */
  Product getCompleteProductDetailByProductCodeInAllProducts(String storeId, String productCode);

  /**
   * fetch master product details using item code
   * @param storeId
   * @param itemCode
   * @return
   */
  ProductMasterDataResponse getMasterProductDetailsByItemCode(String storeId, String itemCode) throws Exception;

  /**
   *
   * @param storeId
   * @param productCode
   * @return
   */
  Product getCompleteProductDetailByProductCodeAndMarkForDeleteFalse(String storeId, String productCode);

  /**
   *
   * @param storeId
   * @param productId
   * @return
   */
  Product getCompleteProductDetailByProductIdAndMarkForDeleteFalse(String storeId, String productId);

  /**
   *
   * @param storeId
   * @param productId
   * @return
   */
  Product getCompleteProductDetailByProductId(String storeId, String productId);

  /**
   *
   * @param storeId
   * @param productCode
   * @return
   */
  Product getProductDetailsWithoutItemsByProductCodeAndMarkForDeleteFalse(String storeId, String productCode);

  /**
   *
   * @param storeId
   * @param productCode
   * @param activated
   * @throws Exception
   */
  void updateProductActivated(String storeId, String productCode, boolean activated) throws Exception;

  /**
   *
   * @param storeId
   * @param id
   * @throws Exception
   */
  void activateProduct(String storeId, String id) throws Exception;

  /**
   *
   * @param storeId
   * @param id
   * @throws Exception
   */
  void deactivateProduct(String storeId, String id) throws Exception;

  /**
   *
   * @param storeId
   * @param productCode
   * @param viewable
   * @throws Exception
   */
  void updateProductViewable(String storeId, String productCode, boolean viewable) throws Exception;

  /**
   *
   * @param product
   * @throws Exception
   */
  void updateProductImage(Product product) throws Exception;

  /**
   * Update product score
   *
   * @param storeId
   * @param productScoreUpdateSwitch
   * @param productScoreBatchSize
   * @param pageable
   * @throws Exception
   */
  void updateProductScore(String storeId, Pageable pageable, boolean productScoreUpdateSwitch,
      int productScoreBatchSize);

  /**
   *
   * @param productAndItemImageRequest
   * @param storeId
   * @param requestId
   * @return
   */
  GdnRestListResponse<LocationPathAndCommonImage> updateProductItemImagesByProductCode(
      ProductItemImageUpdateRequest productAndItemImageRequest, String storeId, String requestId);


  /**
   * Delete original images
   *
   * @param storeId
   * @param productCode
   * @return
   */
  boolean deleteOriginalImagesByProductCode(String storeId, String productCode);

  /**
   * Update vat flag by map of sku code and required vat flag
   *
   * @param storeId
   * @param requestId
   * @param username
   * @param batchVatUpdateRequest
   * @return
   */
  List<BatchVatUpdateResponse> updateVatFlagBySkuCodes(String storeId, String requestId, String username,
      BatchVatUpdateRequest batchVatUpdateRequest);

  /**
   * Update images for a product
   *
   * @param storeId
   * @param productImageEditRequest
   * @return
   */
  Map<String, String> updateImages(String storeId, ProductImageEditRequest productImageEditRequest) throws Exception;

  /**
   * backfill commonImage flag in product images and item images
   *
   * @param storeId
   * @param productCode
   * @param migrationType
   */
  void backfillCommonImageFlagInProductAndItemImages(String storeId, String productCode,
    String migrationType);


  /**
   * Update images for a product
   *
   * @param storeId
   * @param productImageEditRequestList
   * @return
   */
  Map<String, Map<String, String>> updateCommonImages(String storeId, List<ProductImageEditRequest> productImageEditRequestList)
      throws Exception;

  /**
   * Update common images and publish history
   * @param storeId
   * @param username
   * @param productMasterDataUpdateRequest
   * @throws Exception
   */
  Map<String, Map<String, String>> updateCommonImagesAndPublishHistory(
      String storeId, String username, ProductMasterDataUpdateRequest productMasterDataUpdateRequest) throws Exception;

  /**
   * find product images by product code
   *
   * @param storeId
   * @param productCode
   * @return
   */
  List<ImageResponse> findProductImagesByProductCode(String storeId, String productCode, boolean removeOriginalImages);

  /**
   * update product content
   *
   * @param product
   * @param publishProductEvent
   * @param updateFromVendor
   * @throws Exception
   */
  void updateProductContent(Product product, boolean publishProductEvent,
      boolean ignoreSalesCategoryPublish, boolean updateFromVendor) throws Exception;

  /**
   * update product and Mark for Need revision
   *
   * @param product
   * @param
   * @throws Exception
   */
  void updateAndMarkForNeedRevision(Product product) throws Exception;

  /**
   * get product and attribute details by product code
   *
   * @param storeId
   * @param productCode
   * @param inAllProducts
   * @return
   */
  Product getProductAndAttributeDetailsByProductCode(String storeId, String productCode, boolean inAllProducts);

  /**
   * Get product attributes details by productId
   *
   * @param storeId
   * @param productId
   * @return
   */
  List<ProductAttribute> getProductAttributeDetailsByProductId(String storeId, String productId);

  /**
   *
   * @param storeId
   * @param productCode
   */
  void migrateImagesFromGfsToGcsForProducts(String storeId, String productCode);


  /**
   *
   * @param storeId
   * @param productCode
   */
  void migrateFinalImagesFromGfsToGcsForProducts(String storeId, String productCode);

  /**
   * auto fill empty attributes by extracting them from master data
   * @param storeId
   * @param productCode
   * @return
   */
  List<AttributeHistoryResponse> autoFillAttributes(String storeId, String productCode) throws Exception;

  /**
   * Update Review Pending, revised and viewable flags on sending product to need correction
   *
   * @param storeId     storeId
   * @param productCode productCode
   * @param request     NeedRevisionConfigRequest
   */
  void updateFlagsOnNeedCorrectionAndEvictCache(String storeId, String productCode,
    NeedRevisionConfigRequest request) throws Exception;

  /**
   * update Review Pending flag at L3 and Evict product cache
   *
   * @param storeId       storeId
   * @param productCode   productCode
   * @param reviewPending false/true
   */
  void updateProductReviewPendingAndEvictCache(String storeId, String productCode,
    boolean reviewPending) throws Exception;

  /**
   *  @param storeId string
   * @param savedProduct Product
   * @param product Product
   * @param pristineCategory Boolean
   * @param onlyVatChanged boolean
   * @param scoreUpdated boolean
   * @param productDetailChanged boolean
   * @param computeCommonImage boolean
   * @param resetExtractedAttributeValue boolean
   * @param ignoreSalesCategoryPublish boolean
   * @param productAndItemLevelUpdatesDTO ProductAndItemLevelUpdatesDTO
   * @param updatedFields
   */
  Product regenerateProductAndEvictCache(String storeId, Product savedProduct, Product product, Boolean pristineCategory, boolean onlyVatChanged, boolean scoreUpdated, boolean productDetailChanged,
      boolean computeCommonImage, boolean resetExtractedAttributeValue, boolean ignoreSalesCategoryPublish,
      ProductAndItemLevelUpdatesDTO productAndItemLevelUpdatesDTO, Set<String> updatedFields) throws Exception;

  /**
   * @param storeId
   * @param productItemFromRequest
   * @param productCode
   */
  void saveProductItemDetails(String storeId, List<ProductItem> productItemFromRequest, String productCode)
      throws Exception;

  /**
   * mark for delete product
   *
   * @param storeId
   * @param productId
   * @throws Exception
   */
  void markForDeleteProductAndEvictCache(String storeId, String productId) throws Exception;


  /**
   * Combined API to perform Master data, UPC code and Images at product and Item level
   *
   * @param storeId                    string
   * @param product                    Product
   * @param editProductDetailRequest   request
   * @param productCode                not null
   * @param newlySavedItemResponseList newly saved items
   */
  EditProductItemAndImageResponse updateProductMasterDataAndImagesAndUpcCode(Product product,
    EditProductDetailRequest editProductDetailRequest, String productCode, String storeId,
    List<NewlySavedItemResponse> newlySavedItemResponseList) throws Exception;

  /**
   * Update product brand data
   * @param storeId
   * @param productBrandUpdateDTO
   * @return
   */
  ProductBrandUpdateResponseDTO updateProductBrandData(String storeId, ProductBrandUpdateDTO productBrandUpdateDTO);

  /**
   * Process Product Attribute Back Filling for Govt. Compliance
   */
  void processProductAttributeDataBackFilling(
      CommonImageBackfillingEventModel productAttributeDataBackFillingEventModel);


  /**
   * Process ds Attribute mapping to product
   */
  void validateAndProcessProductDsAttributeMapping(ProductSuitabilityEventModel productSuitabilityEventModel)
      throws Exception;

  /**
   * deleteMfdTrueImagesAndAttributes
   *
   * @param deleteMfdTrueImagesAndAttributesEventModel
   */
  void deleteMfdTrueImagesAndAttributes(CommonImageBackfillingEventModel deleteMfdTrueImagesAndAttributesEventModel);

  /**
   * Process Compressed Video after update
   * @param compressedVideoUpdateEventModel event payload
   */
  void processCompressedUpdatedVideo(CompressedVideoUpdateEventModel compressedVideoUpdateEventModel)
    throws Exception;

  /**
   * Update master data and evict cache
   *
   * @param storeId
   * @param username
   * @param productMasterDataUpdateRequest
   */
  void updateMasterDataAndEvictCache(String storeId, String username,
      ProductMasterDataUpdateRequest productMasterDataUpdateRequest) throws Exception;

  /**
   * Get product master data by product code
   *
   * @param storeId
   * @param productCodes
   */
  List <ProductMasterDataItemResponse> getProductMasterDataByProductCode(String storeId, ProductCodesRequest productCodes) throws Exception;

  /**
   * Update product-brand data with brand data if required
   *
   * @param storeId
   * @param productBrandDataUpdateRequest
   * @throws Exception
   */
  ProductBrandUpdateResponse updateProductBrandDataWithBrandInfo(String storeId,
      ProductBrandDataUpdateRequest productBrandDataUpdateRequest) throws Exception;
}
