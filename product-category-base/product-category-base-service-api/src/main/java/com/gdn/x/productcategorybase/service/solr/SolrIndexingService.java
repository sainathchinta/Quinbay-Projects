package com.gdn.x.productcategorybase.service.solr;

public interface SolrIndexingService {

  /**
   * Full reindex brand collection
   *
   * @param storeId
   * @throws Exception
   */
  void fullReindexBrandCollection(String storeId) throws Exception;

  /**
   * Partial reindex brand collection
   *
   * @param storeId
   * @throws Exception
   */
  void partialReindexBrandCollection(String storeId) throws Exception;

  /**
   * Reindex brand collection by brand wip code
   *
   * @param storeId
   * @param brandRequestCode
   * @throws Exception
   */
  void reindexBrandCollectionByBrandRequestCode(String storeId, String brandRequestCode) throws Exception;

  /**
   * Full Reindex pcb collection
   * @param storeId
   */
  void fullReindexPCBCollection(String storeId) ;

  /**
   * Delta Reindex for pcb collection
   * @param storeId
   * @throws Exception
   */
  void deltaReindexPCBCollection(String storeId) throws Exception;

  /**
   * Delta reindex by productCode
   *
   * @param storeId
   * @param productCode
   * @throws Exception
   */
  void deltaReindexPCBCollectionByProductCode(String storeId, String productCode) throws Exception;

  /**
   * Category based reindex for pcb collection
   * @param storeId
   * @param categoryCode
   */
  void categoryBasedReindexPCBCollection(String storeId, String categoryCode) throws Exception;
}
