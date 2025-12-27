package com.gdn.mta.product.service.solr;

import java.util.List;

import com.gdn.mta.domain.event.modal.SolrProductCollectionUpdateEvent;
import com.gdn.x.productcategorybase.dto.response.PredefinedAllowedAttributeValueResponse;

public interface SolrActiveProductCollectionService {

  /**
   * Triggering delete from solr
   * 
   * @param storeId must not be null
   * @param productCode must not be null
   */
  void deleteFromSolrProductCollection(String storeId, String productCode);

  /**
   * Adding product collection document into solr
   *
   * @param solrProductCollectionUpdateEvent must not be null
   */
  void addSolrProductCollectionDocument(SolrProductCollectionUpdateEvent solrProductCollectionUpdateEvent);

  /**
   * Triggering sync inactive product solr
   * 
   * @param storeId must not be null
   */
  void syncInactiveProductCollection(String storeId);


  /**
   * Get all active brands by category codes
   *
   * @param categoryCodes
   * @return
   */
  List<PredefinedAllowedAttributeValueResponse> getAllActiveBrandsByCategoryCodes(List<String> categoryCodes);

  /**
   * delete from solr product collection by document id
   *
   * @param DocumentId
   */
  void deleteSolrProductCollectionByDocumentId(String DocumentId);

  /**
   * Delete product collection document
   *
   * @param documentId
   */
  void deleteSolrProductCollectionDocument(String documentId);
}
