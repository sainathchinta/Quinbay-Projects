package com.gdn.mta.product.service;

import java.util.Date;
import java.util.List;
import java.util.Map;

import jakarta.persistence.PersistenceException;

import com.gda.mta.product.dto.ProductVariantPriceStockAndImagesRequest;
import com.gda.mta.product.dto.response.InProgressProductResponse;
import com.gda.mta.product.dto.response.ProductBusinessPartnerAndItemViewConfigDto;
import com.gdn.mta.product.entity.ProductItemBusinessPartner;
import com.gdn.mta.product.entity.ProductLevel3;
import com.gdn.mta.product.entity.ProductSkuBusinessPartnerDTO;
import com.gdn.partners.pbp.model.productlevel3.CreateProductLevel3Response;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;

import com.gda.mta.product.dto.ProductItemBusinessPartnerRequest;
import com.gdn.common.base.service.GdnBaseService;
import com.gdn.mta.product.entity.ItemFlagDetails;
import com.gdn.mta.product.entity.ProductBusinessPartner;
import com.gdn.mta.product.entity.ProductBusinessPartnerAttribute;
import com.gdn.mta.product.entity.RejectedSkuProductCollection;
import com.gdn.x.product.rest.web.model.request.ItemViewConfigAndItemSkuRequest;
import com.gdn.x.product.rest.web.model.response.ItemSummaryDetailResponse;
import com.gdn.x.product.rest.web.model.response.ProductL3Response;
import com.gdn.x.productcategorybase.dto.response.ProductDetailResponse;
import com.gdn.x.productcategorybase.entity.ProductAttribute;

public interface ProductBusinessPartnerService extends GdnBaseService<ProductBusinessPartner> {

  String ERR_CODE_NO_AVAILABLE_VALID_ITEMS = "NO_VALID_PRODUCTS_TO_COPY";

  Page<ProductBusinessPartner> findByActivatedTrue(String storeId, Pageable pageable) throws Exception;

  Page<ProductBusinessPartner> findByBusinessPartnerId(String storeId, String businessPartnerId, Pageable pageable);

  Page<ProductBusinessPartner> findActiveProductsByBusinessPartnerId(String storeId, String businessPartnerId, Pageable pageable);

  /**
   * Find ProductState By StoreId And ItemSku
   *
   * @param storeId
   * @param itemSku
   * @return
   * @throws Exception
   */
  String findProductStateByStoreIdAndItemSku(String storeId, String itemSku) throws Exception;

  String validateProductSku(String gdnProductSku) throws Exception;

  Page<ProductBusinessPartner> findByPickupPointId(String storeId, String pickupPointId, Pageable pageable)
    throws PersistenceException;

  void retrySave(ProductBusinessPartner productBusinessPartner) throws Exception;

  List<ProductBusinessPartner> findByStoreIdAndProductIdAndMarkForDeleteFalse(String storeId, String productId)
    throws Exception;

  /**
   * Fetch L3 data by product id
   *
   * @param storeId
   * @param productId
   * @return
   * @throws Exception
   */
  List<ProductBusinessPartner> findByStoreIdAndProductId(String storeId, String productId);

  /**
   * Fetch inactive L3 by product id
   *
   * @param storeId
   * @param productId
   * @return
   * @throws Exception
   */
  List<ProductBusinessPartner> findInactiveProductBusinessPartnersOfActiveL1(String storeId, String productId) throws Exception;

  String saveWithActivatedFalse(ProductBusinessPartner productBusinessPartner) throws Exception;

  /**
   * Save product business partner
   *
   * @param productBusinessPartner
   * @throws Exception
   */
  void saveProductBusinessPartner(ProductBusinessPartner productBusinessPartner) throws Exception;

  Date getExpectedActivationDateByCategoryCode(ProductDetailResponse productDetailResponse, Date submitDate);

  CreateProductLevel3Response create(String storeId, ProductBusinessPartner productBusinessPartner,
      boolean skipNotification, List<ProductItemBusinessPartnerRequest> businessPartnerRequestList)
      throws Exception;

  /**
   * Retry L3 L4 creation by id or entity
   *
   * @param storeId
   * @param productBusinessPartnerId
   * @param productBusinessPartner
   * @throws Exception
   */
  void retryCreate(String storeId, String productBusinessPartnerId, ProductBusinessPartner productBusinessPartner)
    throws Exception;

  /**
   * save product business partner mapping to product
   *
   * @param productBusinessPartner product business-partner, must not null
   * @param productDetailResponse  product detail response, must not null
   *
   * @param isMPPFlow
   * @return
   *
   * @throws Exception
   */
  ProductBusinessPartner saveBusinessPartner(ProductBusinessPartner productBusinessPartner,
      ProductDetailResponse productDetailResponse, boolean isMPPFlow) throws Exception;

  List<ProductBusinessPartnerAttribute> getSkuValueTrueAttributeList(List<String> attributeIdList,
    String productId) throws Exception;

  Integer getProductTypeBasedOnProductId(String productId) throws Exception;

  Integer getProductTypeBasedOnProductCodeOrId(String productCode, String productId);

  Integer getMinimumStockByGdnProductItemSku(String itemSku) throws Exception;

  void updateMinimumStockByGdnProductItemSku(String itemSku, Integer minimumStock) throws Exception;

  /**
   * @param storeId
   * @param businessPartnerId
   * @param pageable
   * @param searchCriteria
   *
   * @return
   *
   * @throws Exception
   */
  Page<RejectedSkuProductCollection> findRejectedProductsByBusinessPartnerId(String storeId,
    String businessPartnerId, Pageable pageable, String searchCriteria) throws Exception;

  /**
   * @param storeId
   * @param businessPartnerId
   * @param pageable
   * @param merchantSku
   *
   * @return
   *
   * @throws Exception
   */
  Page<RejectedSkuProductCollection> findRejectedProductsByBusinessPartnerIdAndMerchantSku(String storeId,
    String businessPartnerId, Pageable pageable, String merchantSku) throws Exception;

  /**
   * Mark all the items associated with a product as unbuyable and undisplayable
   *
   * @param productId
   */
  void markItemsAsUnBuyableAndUnViewable(String productId);

  /**
   * Update sku value true values of descriptive values in prdProductBusinessPartnerAttribute
   *
   * @param productAttributeList
   * @param productId
   */
  void updateSkuValueTrueInProductBusinessPartnerAttribute(List<ProductAttribute> productAttributeList,
    String productId);

  /**
   * delete old items entries on resubmission
   *
   * @param businessPartnerId
   * @param productId
   */
  void markItemsAsDeletedOnProductResubmission(String businessPartnerId, String productId);

  /**
   * Create duplicate item (L3 & L4) using existing item, required details will be copied from existing item
   * @param storeId             store identifier
   * @param username            username
   * @param businessPartnerCode business partner code
   * @param pickupPointCode     pickup point code
   * @param isRetryAttempt      is retry attempt
   * @param processId           process Id
   * @param itemSkus            items to be copied
   * @param linkedPartnerCode   source business partner code for product copying
   */
  void copy(String storeId, String username, String businessPartnerCode, String pickupPointCode, boolean isRetryAttempt,
    String processId, Map<String, List<String>> itemSkus, String linkedPartnerCode);

  /**
   * Create duplicate item (L3 & L4) using existing item, required details will be copied from existing item for all
   * active products of source merchant
   *
   * @param storeId           store identifier
   * @param username          username
   * @param linkedPartnerCode source partner code
   * @param partnerCode       business partner code
   * @param pickupPointCode   pickup point code
   */
  void copyAllProducts(String storeId, String username, String linkedPartnerCode, String partnerCode,
    String pickupPointCode) throws Exception;

  /**
   * send notifications for processes finished copying products
   *
   *  @param storeId           store identifier
   */
  void notifyForProductCopyingProcess(String storeId);

  /**
   * reset productSyncStatus to FAIL for all the records IN_PROGRESS updated before sync retry duration
   *
   *  @param storeId           store identifier
   */
  void resetProductItemSyncStatus(String storeId);


  /**
   * fetch the count of rejected products for the business partner
   *
   *  @param storeId
   *  @param businessPartnerId
   */
  long countRejectedProductsByBusinessPartnerId(String storeId, String businessPartnerId);


  /**
   * fetch the count of suspended items for the business partner
   *
   *  @param storeId
   *  @param requestId
   *  @param userName
   *  @param businessPartnerCode
   */
  long countSuspendedProductsByBusinessPartnerCode(String storeId, String requestId, String userName,
      String businessPartnerCode) throws Exception;

  /**
   * fetch rejected products by merchantId in specific order
   * @param storeId
   * @param businessPartnerId
   * @param pageable
   * @param searchCriteria
   * @param orderBy
   * @param sortBy
   * @return
   */
  Page<RejectedSkuProductCollection> findRejectedProductsByBusinessPartnerIdWithOrderByAndSortBy(String storeId,
      String businessPartnerId, Pageable pageable, String searchCriteria, String orderBy, String sortBy);

  /**
   * Check if any product is mapped to merchant by merchant code
   *
   * @param storeId
   * @param merchantCode
   * @return
   */
  boolean isProductMappedToMerchant(String storeId, String merchantCode);

  /**
   * Get item flag details by productId
   *
   * @param productId
   * @return
   */
  List<ItemFlagDetails> getAllItemSkusViewConfigByProductId(String productId);

  /**
   * API to fetch the minimum price from the configuration.
   *
   */
  Integer getMinimumPrice(String storeId);

  /**
   * API to fetch the product skus from productCode
   *
   * @param productCode
   */
  List<String> getProductSkusByProductCode(String productCode);

  /**
   * get gdn sku list by product code
   *
   * @param productCodeList
   * @return
   */
  List<ProductSkuBusinessPartnerDTO> getGdnSkuListByProductCodes(List<String> productCodeList);

  /**
   * @param productSku       String
   * @param productName      String
   * @param categoryCode     String
   * @param categoryName     String
   * @param sizeChartCode    String
   * @param updatedBy        username
   * @param sizeChartChanged boolean
   * @param brandName
   */
  void updateProductMasterData(String productSku, String productName, String categoryCode,
    String categoryName, String sizeChartCode, String updatedBy, boolean sizeChartChanged,
      String brandName);

  /**
   * update Size Chart Details
   *
   * @param productSku       not empty
   * @param sizeChartCode    not empty
   * @param updatedBy        not empty
   * @param sizeChartChanged boolean
   * @param brandUpdated
   * @param brand
   * @return
   */
  void updateSizeChartDetailsAndBrandDetails(String productSku, String sizeChartCode, String updatedBy,
    boolean sizeChartChanged, boolean brandUpdated, String brand);

  /**
   * Find products by product skus
   *
   * @param storeId
   * @param productSkuList
   * @return
   */
  List<ProductBusinessPartner> findByProductSkuList(String storeId, List<String> productSkuList);

  /**
   * update product business partner state
   * @param productBusinessPartner
   * @param isTakeDown
   * @param categoryCode
   * @return
   */
  ProductBusinessPartner updateProductBusinessPartnerState(ProductBusinessPartner productBusinessPartner, boolean isTakeDown,
      String categoryCode, ProductL3Response savedProductData);

  /**
   * update product item business partner state for taken down
   * @param itemSummaryDetailResponses
   * @param itemViewConfigAndItemSkuRequests
   * @param productBusinessPartner
   * @return
   */
  ProductBusinessPartnerAndItemViewConfigDto updateProductItemBusinessPartnerStateTakeDownTrue(
      List<ItemSummaryDetailResponse> itemSummaryDetailResponses, List<ItemViewConfigAndItemSkuRequest> itemViewConfigAndItemSkuRequests,
      ProductBusinessPartner productBusinessPartner) throws Exception;

  /**
   * update product item business partner state for activation
   * @param itemViewConfigAndItemSkuRequests
   * @param productBusinessPartner
   * @param businessPartnerActive
   * @return
   */
  List<ItemViewConfigAndItemSkuRequest> updateProductItemBusinessPartnerStateTakeDownFalse(
      List<ItemViewConfigAndItemSkuRequest> itemViewConfigAndItemSkuRequests,
      ProductBusinessPartner productBusinessPartner, boolean businessPartnerActive);

  /**
   * Fetch In progress products by seller code
   *
   * @param storeId
   * @param merchantCode
   * @param inProgressState
   * @return
   */
  List<InProgressProductResponse> findByStoreIdAndBusinessPartnerIdAndStateAndMarkForDeleteFalse(
    String storeId, String merchantCode, List<String> inProgressState);

  /**
   *
   * update productName free sample and in-store flags in need revision flow
   *
   * @param productSku
   * @param product
   * @return
   */
  void updateNeedRevisionL3Details(String productSku, ProductLevel3 product);

  /**
   * generating new Item business partner data new newly added variants
   *
   * @param newlyAddedProductItemRequests
   * @return
   */
  List<ProductItemBusinessPartner> generateNewProductItemBusinessPartnerData(List<ProductVariantPriceStockAndImagesRequest>
      newlyAddedProductItemRequests, Integer productType);

  /**
   * find business partner data by product id
   *
   * @param storeId
   * @param productId
   * @return
   */
  ProductBusinessPartner findFirstByStoreIdAndProductId(String storeId, String productId);

  /**
   * delete entries from product business partner repo by storeId and productId
   * @param storeId
   * @param productId
   */
  void deleteProductBusinessPartnerByStoreIdAndProductId(String storeId, String productId);

  /**
   * find First ProductBusinessPartner By ProductSku
   * @param productSku not null
   * @return product business partner
   */
  ProductBusinessPartner findFirstByProductSku(String productSku);

  /**
   * Find by productCode
   *
   * @param productCode
   * @return
   */
  List<ProductBusinessPartner> findByProductCode(String productCode);

  /**
   * Update brand data by productCode
   *
   * @param productBusinessPartners
   * @param brandName
   */
  void updateBrand(List<ProductBusinessPartner> productBusinessPartners, String brandName);
}
