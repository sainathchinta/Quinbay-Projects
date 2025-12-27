package com.gdn.x.product.dao.solr.api;

import java.io.IOException;
import java.util.List;

import org.apache.solr.client.solrj.SolrClient;
import org.apache.solr.client.solrj.SolrServerException;
import org.apache.solr.common.SolrInputDocument;

public interface SolrIndexingRepository {

  /**
   * Get documents from sourceCollection and update in destinationCollection
   *
   * @param sourceCollection
   * @param destinationCollection
   * @throws Exception
   */
  void updateAll(SolrClient sourceCollection, SolrClient destinationCollection) throws Exception;


  /**
   * Get documents from sourceCollection and update in destinationCollection (L3)
   *
   * @param storeId
   * @param sourceCollection
   * @param destinationCollection
   * @param batchSize
   * @param inventoryBatchSize
   * @throws Exception
   */
  void copyProductsToL3Collection(String storeId, SolrClient sourceCollection,
      SolrClient destinationCollection, List<String> categoryCodes, String sortOrder, int batchSize,
      int inventoryBatchSize)
      throws Exception;

  /**
   * Update solr documents by solrClient
   *
   * @param solr
   * @param solrInputDocuments
   */
  void updateBatch(SolrClient solr, List<SolrInputDocument> solrInputDocuments)
      throws IOException, SolrServerException;
}
