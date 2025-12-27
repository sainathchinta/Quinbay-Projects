package com.gdn.x.product.service.api;

import java.util.List;
import java.util.Map;

import org.springframework.data.domain.Pageable;

import com.gdn.x.product.enums.ProductReindexStatus;
import com.gdn.x.product.model.entity.Product;

public interface ReindexService {

  void reindexFull(String requestId, String username, String storeId) throws Exception;

  /**
   * Delta Reindex
   * @param storeId
   * @param requestId
   * @param username
   * @param indexFrom
   * @param indexTill
   */
  void deltaReindex(String storeId, String requestId, String username, String indexFrom, String indexTill)
      throws Exception;

  void deltaReindexItemSkus(String storeId) throws Exception;

  void deltaReindexItemSkuByPristine(String storeId, Pageable pageable) throws Exception;

  void reindexFullSimple(String requestId, String username, String storeId) throws Exception;

  void reindexOldProduct(String requestId, String username, String storeId) throws Exception;

  void reindexSolrAndClearCacheByProductSkus(String requestId, String username, String storeId,
      List<String> productSku) throws Exception;

  void reindexSolrAndClearCacheByProductSkusSimple(String requestId, String username,
      String storeId, List<String> productSku) throws Exception;

  /**
   * Get documents from sourceCollection and update in destinationCollection
   *
   * @param sourceCollection
   * @param destinationCollection
   * @throws Exception
   */
  void updateAll(String sourceCollection, String destinationCollection) throws Exception;

  /**
   * Get documents from sourceCollection and update in destinationCollection(L3)
   *
   * @param sourceCollection
   * @param destinationCollection
   * @param sortOrder
   * @param storeId
   * @throws Exception
   */
  void copyProductsToL3Collection(String sourceCollection, String destinationCollection, List<String> categoryCodes,
      String sortOrder, String storeId) throws Exception;

  /**
   *
   * @param storeId
   * @throws Exception
   */
  void reindexDeferredItemsFirstPage(String storeId) throws Exception;

  /**
   * Deferred reindex by reindex type
   *
   * @param storeId
   * @throws Exception
   */
  void reindexDeferredItemsByReindexType(String storeId, String reindexType, String orderBy, String status) throws Exception;

  /**
   * Delta Reindex
   * @param storeId
   * @param indexFrom
   * @param indexTill
   */
  void deltaReindexToL3Collection(String storeId, String indexFrom, String indexTill)
      throws Exception;

  /**
   * Populate new fields on L3 reindex collection
   *
   * @param storeId
   * @param maxReindexCount
   * @param reindexType
   */
  void populateL3CollectionWithNewFields(String storeId, int maxReindexCount, String reindexType)
      throws InterruptedException;

  /**
   * Reindex product sku with new fields by input
   *
   * @param productSkuList
   */
  void reindexNewFieldsToL3Collection(List<String> productSkuList);

  /**
   * Reindex product sku with archive and o2o flag
   *
   * @param productSkuList
   */
  void reindexNewFlagValuesToL3Collection(List<String> productSkuList);

  /**
   * Update status in reindex l3 collection
   *
   * @param productSkuList
   * @param status
   */
  void updateStatusInL3ReindexCollection(List<String> productSkuList, ProductReindexStatus status);

  /**
   * Reindex offline items
   *
   * @param itemSkuList
   */
  void reindexOfflineItems(List<String> itemSkuList);

  /**
   * Reindex items
   *
   * @param itemSkuList
   */
  void reindexItems(List<String> itemSkuList);

  /**
   * Reindex pending l3 collection
   *
   * @param productSkuList
   */
  void reindexPendingL3Collection(List<String> productSkuList);

  /**
   * Reindex L3 solr and database entries
   *
   * @param productSkuList
   * @param productSkuAndProductMap
   */
  void reindexPendingL3SolrAndDatabase(List<String> productSkuList, Map<String, Product> productSkuAndProductMap);

  /**
   * Delete documents from L3,L4 Solr
   * @param itemSkus
   */
  void deleteProductAndItemsFromSolr(List<String> itemSkus);
}
