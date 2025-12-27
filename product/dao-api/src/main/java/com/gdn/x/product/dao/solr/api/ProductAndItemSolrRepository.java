package com.gdn.x.product.dao.solr.api;

import java.io.IOException;
import java.util.List;
import java.util.Set;

import org.apache.solr.client.solrj.SolrServerException;

import com.gdn.x.product.model.solr.ProductAndItemSolr;

public interface ProductAndItemSolrRepository extends ProductAndItemSolrRepositoryCustom {

  List<ProductAndItemSolr> findByProductCode(String productCode);

  List<ProductAndItemSolr> findByProductSku(String productSku);

  Set<ProductAndItemSolr> findByStoreIdAndMerchantCodeAndOff2OnChannelActiveAndMarkForDeleteFalse(
      String storeId, String merchantCode, boolean off2OnChannelActive);

  List<ProductAndItemSolr> findByStoreIdAndPickupPointCodeAndMarkForDeleteFalse(String storeId,
      String pickupPointCode);

  List<ProductAndItemSolr> findByStoreIdAndProductCatentryIdInAndMarkForDeleteFalse(String storeId,
      Set<String> productCatentryId);

  List<ProductAndItemSolr> findByStoreIdAndProductCodeAndMerchantCodeAndMarkForDeleteFalseAndIsSynchronizedTrue(
      String storeId, String productCode, String merchantCode);

  List<ProductAndItemSolr> findByStoreIdAndProductCodeInAndMarkForDeleteFalse(String storeId,
      Set<String> productCode);

  List<ProductAndItemSolr> findByStoreIdAndProductCodeInAndMarkForDeleteFalseAndIsSynchronizedTrue(
      String storeId, Set<String> productCode);

  List<ProductAndItemSolr> findByStoreIdAndProductSku(String storeId, String productSku,
    String merchantCode);

  List<ProductAndItemSolr> findByStoreIdAndProductSkuAndMarkForDeleteFalse(String storeId,
      String productSku, String merchantCode);

  List<ProductAndItemSolr> findByStoreIdAndProductSkuInAndMarkForDeleteFalse(String storeId,
      List<String> productSku);

  List<ProductAndItemSolr> findByStoreIdAndProductSkuInAndMarkForDeleteFalse(String storeId,
      Set<String> productSku);

  List<ProductAndItemSolr> findByTicketTemplateCodeAndMarkForDeleteFalse(String ticketTemplateCode);

  ProductAndItemSolr findFirstByStoreIdAndProductCatentryIdAndMarkForDeleteFalse(String storeId,
      String productCatentryId);

  List<ProductAndItemSolr> findItemSkuAndCodeByStoreIdAndItemSkuIn(String storeId,
      Set<String> itemSku);

  Set<String> getItemSkusByPristineId(String storeId, String pristineId) throws IOException, SolrServerException;

  ProductAndItemSolr findFirstByStoreIdAndItemSku(String storeID, String itemSku);

  List<ProductAndItemSolr> findAllIdsByStoreIdAndMerchantCodeAndMarkForDeleteFalse(String storeId, String merchantCode);

  void save(ProductAndItemSolr productAndItemSolr);

  void updateCncActivatedByMerchantCodeSolr(String storeId, String merchantCode, boolean cncActivated);

  /**
   * update CncActivated By ProductSkuSet in Solr
   *
   * @param storeId
   * @param productSkuSet
   * @param cncActivated
   * @param merchantCode
   */
  void updateCncActivatedByProductSkuSetSolr(String storeId, Set<String> productSkuSet, boolean cncActivated,
    String merchantCode);

  ProductAndItemSolr findOne(String itemSku, String merchantCode);

  /**
   * find whether the brand used in a product or not, from L3 solr
   *
   * @param storeId
   * @param brand
   * @return
   */
  Long findByStoreIdAndBrandAndMarkForDeleteFalse(String storeId, String brand);

  /**
   *get item count based on category codes and brands
   *
   *
   * @param storeId
   * @param categoryCodesInBatches
   * @param brands
   * @param merchantCode
   * @return
   */
  Long getCountByStoreIdAndCategoryAndBrandAndMarkForDeleteFalse(String storeId,
      List<List<String>> categoryCodesInBatches,List<String> brands, String merchantCode);

  Long getCountByStoreIdAndPickupPointCodeAndMarkForDeleteFalse(String storeId, String pickupPointCode);

}