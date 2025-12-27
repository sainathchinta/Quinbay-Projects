package com.gdn.x.productcategorybase.repository;

import java.io.IOException;
import java.util.List;

import org.apache.solr.client.solrj.SolrServerException;

import com.gdn.x.productcategorybase.entity.solr.SolrPcbProductModel;

public interface SolrPcbRepository {

  /**
   * Add to pcb_collection in batch
   * @param solrPcbProductModelList
   */
  void addProductListToPcbCollection(List<SolrPcbProductModel> solrPcbProductModelList);

  /**
   * Delete from pcb collection
   * @param ids
   */
  void deleteProductListFromPcbCollection(List<String> ids);

  /**
   * Delete all the documents from solr
   *
   */
  void deleteAllDocumentsFromSolr() throws IOException, SolrServerException;

}
