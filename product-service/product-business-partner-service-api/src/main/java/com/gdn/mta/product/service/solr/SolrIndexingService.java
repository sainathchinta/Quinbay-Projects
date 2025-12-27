package com.gdn.mta.product.service.solr;

import java.text.ParseException;
import java.util.List;

public interface SolrIndexingService {

  /**
   * Get documents from sourceCollection and update in destinationCollection
   *
   * @param sourceCollection
   * @param destinationCollection
   * @throws Exception
   */
  void updateAll(String sourceCollection, String destinationCollection) throws Exception;

  /**
   * reindex the variant history based onm product skus
   * @param storeId
   * @param productSkus
   */
  void reindexProductHistoryByProductSkus(String storeId, List<String> productSkus);

  /**
   * full reindex history collection
   * @param storeId
   * @param startTime
   * @param endTime
   */
  void deltaReindexHistoryCollection(String storeId, String startTime, String endTime);

  /**
   * delta reindex for prd product collection
   *
   * @param storeId
   * @param indexFrom
   * @param indexTill
   * @throws ParseException
   */
  void deltaReindexPrdProductCollection(String storeId, String indexFrom, String indexTill) throws ParseException;
}
