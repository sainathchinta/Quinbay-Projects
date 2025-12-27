package com.gdn.mta.product.repository;

import org.apache.solr.client.solrj.SolrClient;

public interface SolrIndexingRepository {

  /**
   * Get documents from sourceCollection and update in destinationCollection
   *
   * @param sourceCollection
   * @param destinationCollection
   * @throws Exception
   */
  void updateAll(SolrClient sourceCollection, SolrClient destinationCollection) throws Exception;
}
