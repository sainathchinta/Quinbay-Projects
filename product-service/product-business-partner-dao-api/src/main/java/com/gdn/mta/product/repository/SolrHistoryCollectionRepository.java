package com.gdn.mta.product.repository;

import java.io.IOException;
import java.util.Date;
import java.util.List;

import com.gda.mta.product.dto.HistoryUpdateRequest;
import org.apache.solr.client.solrj.SolrServerException;
import org.apache.solr.common.SolrDocumentList;
import org.apache.solr.common.SolrInputDocument;

import com.gda.mta.product.dto.HistoryRequest;

public interface SolrHistoryCollectionRepository {

  /**
   * save/update solr document
   *
   * @param solrDocumentList
   */
  void addDocument(List<SolrInputDocument> solrDocumentList);

  /**
   * find variant history based on productSku and keyword
   *
   * @param storeId
   * @param historyRequest
   * @param page
   * @param size
   * @return
   */
  SolrDocumentList findProductHistoryByProductSkuAndKeyword(String storeId, HistoryRequest historyRequest, int page,
      int size) throws IOException, SolrServerException;

  /**
   * Delete solr documents by auditTrailIds
   *
   * @param days
   * @param accessTime
   * @throws Exception
   */
  void deleteHistoryFromSolr(int days, Date accessTime) throws Exception;

  /**
   * @param storeId
   * @param historyUpdateRequest
   * @param page
   * @param size
   * @return
   */
  SolrDocumentList findProductUpdateHistoryByRequest(String storeId, HistoryUpdateRequest historyUpdateRequest, int page, int size)
      throws SolrServerException, IOException;
}
