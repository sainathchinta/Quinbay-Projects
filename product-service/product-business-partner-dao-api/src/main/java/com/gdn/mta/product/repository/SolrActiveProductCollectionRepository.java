package com.gdn.mta.product.repository;

import java.util.List;
import java.util.Set;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;

import com.gda.mta.product.dto.ProductFilterRequest;
import com.gdn.mta.product.valueobject.SolrCategoryCodeDTO;
import com.gdn.mta.product.valueobject.SolrProductCodeDTO;
import com.gdn.mta.product.valueobject.SolrProductCollectionDTO;

/**
 * Created by virajjasani on 09/12/16.
 */
public interface SolrActiveProductCollectionRepository {

  /**
   *  repository method to retrieve list of product collection from solr documents
   *
   * @param storeId store id
   * @param keyword query keyword
   * @param categoryCode category code
   * @param reviewPending reviewPending flag
   * @param pageable paging
   * @return SolrProductCollectionDTO solr document object
   * @throws Exception
   */
  Page<SolrProductCollectionDTO> getProductCollectionListFromSolrCollection(String storeId, String keyword,
      String categoryCode, Boolean reviewPending, String sortBy, Pageable pageable) throws Exception;

  /**
   * Get active product codes from solr
   *
   * @param storeId
   * @param keyword
   * @param categoryCode
   * @param reviewPending
   * @param sortBy
   * @param pageable
   * @return
   * @throws Exception
   */
  List<String> getProductCodesListFromSolr(String storeId, String keyword, String categoryCode, Boolean reviewPending,
      String sortBy, Pageable pageable);

  Integer getProductCountByStoreId(String storeId) throws Exception;

  Set<SolrCategoryCodeDTO> getCategoryCodesSolrByKeyword(String keyword, Integer rows, String businessPartnerCode) throws Exception;

  SolrProductCodeDTO getProductCodesFromSolrByKeywordAndCategoryCode(
      String keyword, String categoryCode, Pageable pageable) throws Exception;

  SolrProductCodeDTO findProductCodesByKeywordAndCategoryCodes(String keyword, List<String> categoryCodes,
      Pageable pageable) throws Exception;

  /**
   * add solr document for product collection
   *
   * @param solrProductCollectionDTO solr document object
   */
  void addSolrProductCollectionDocument(SolrProductCollectionDTO solrProductCollectionDTO);

  /**
   * Get all active brands by category codes
   *
   * @param categoryCodes
   * @return
   */
  List<String> getAllActiveBrandsByCategoryCodes(List<String> categoryCodes);

  /**
   * Respository call to fetch solr documents for productFilterRequest
   *
   * @param storeId
   * @param productFilterRequest
   * @param pageable
   * @return
   * @throws Exception
   */
  Page<SolrProductCollectionDTO> getProductCollectionListForFilterRequest(String storeId,
      ProductFilterRequest productFilterRequest, Pageable pageable) throws Exception;

  /**
   * delete from product collection by document id
   *
   * @param documentId
   */
  void deleteSolrProductCollectionByDocumentId(String documentId);
}
