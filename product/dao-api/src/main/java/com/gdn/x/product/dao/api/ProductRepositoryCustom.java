package com.gdn.x.product.dao.api;

import java.util.Collection;
import java.util.Date;
import java.util.List;
import java.util.Set;

import com.gdn.x.product.model.entity.Product;
import com.gdn.x.product.model.entity.SalesCatalog;

public interface ProductRepositoryCustom {

  List<Product> findByStoreIdAndProductSkusAndMarkForDeleteFalse(String storeId,
      List<String> productSkus, String... includes);

  List<Product> findSyncProductByStoreIdAndProductCodesAndMarkForDeleteFalse(String storeId,
      Set<String> productCodes, String... includes);

  void saveWithCustomAuditValue(Product product);

  Product updateFieldByProductSku(String storeId, String productSku, String fieldName,
      Object value);

  void updateCncActivatedByMerchantCode(String storeId, String merchantCode, boolean cncActivated,
      String username);

  /**
   * update CncActivated By ProductSku
   *  @param storeId
   * @param productSkuSet
   * @param cncActivated
   * @param username
   */
  List<Product> updateCncActivatedByProductSkusMarkForDeleteFalse(String storeId, Set<String> productSkuSet, boolean cncActivated, String username);

  Product updateOff2OnItemCountIncrementByProductSku(String storeId, String productSku,
      int increment);

  Product updateOff2OnItemCountIncrementByProductSkuAndFlag(String storeId, String productSku, boolean activate,
      int itemsCount, String userName);

  Product updateSalesCatalog(String storeId, String productSku, List<SalesCatalog> catalogs, Date updatedDate);

  /**
   * update pickupPointCodes by productsku
   *
   * @param storeId
   * @param username
   * @param productSku
   * @param pickupPointCodes
   * @param isFbbActivated
   * @return
   */
  Product updatePickupPointCodes(String storeId, String username, String productSku,
      Set<String> pickupPointCodes, boolean isFbbActivated);

  /**
   * Get Products with only specified fields by productCodes
   * @param storeId
   * @param productCodes
   * @param includes
   * @return
   */
  List<Product> findByStoreIdAndProductCodes(String storeId, Set<String> productCodes,
      String... includes);

  /**
   * Get Products with only specified fields by productSkus
   * @param storeId
   * @param productSkus
   * @param includes
   * @return
   */
  List<Product> findByStoreIdAndProductSkus(String storeId, Set<String> productSkus,
      String[] includes, boolean showDeleted);

  /**
   * Update fbb flag at L3 by product sku
   *
   * @param storeId
   * @param productSku
   * @param fbbActivated
   * @return
   */
  Product updateFbbFlagAtProduct(String storeId, String productSku, boolean fbbActivated);

  /**
   * Get product using storeId and productSku and additional param for data source.
   * @param storeId must not be blank
   * @param productSku must not be blank
   * @param readFromPrimary
   * @return Product
   */
  Product findProductByStoreIdAndProductSku(String storeId, String productSku, boolean readFromPrimary);


  /**
   *
   * @param storeId
   * @param productSku
   * @param readFromPrimary
   * @return
   */
  Product findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(String storeId, String productSku, boolean readFromPrimary);

  /**
   * Get Products by storeId,productSkus
   * @param storeId
   * @param productSku
   * @return
   */
  List<Product> findProductByStoreIdAndProductSkuIn(String storeId, Collection<String> productSku,  boolean readFromPrimary);

}
