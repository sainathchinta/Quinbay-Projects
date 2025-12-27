package com.gdn.mta.product.repository;

import java.io.IOException;
import java.util.Map;

import org.apache.solr.client.solrj.SolrServerException;
import org.apache.solr.client.solrj.impl.CloudSolrClient;
import org.apache.solr.client.solrj.response.QueryResponse;
import org.apache.solr.common.SolrDocumentList;
import org.apache.solr.common.SolrInputDocument;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;

import com.gda.mta.product.dto.DalamProductListRequest;
import com.gdn.mta.product.entity.ProductBusinessPartnerMapper;
import com.gdn.mta.product.valueobject.SolrProductCollectionDTO;
import com.gdn.mta.product.valueobject.SummaryFilterServiceRequest;

public interface SolrReviewProductCollectionRepository {

  /**
   * Get filter count by store id , activated and viewable flag
   *
   * @param storeId
   * @param activated
   * @param viewable
   * @return
   * @throws IOException
   * @throws SolrServerException
   */
  QueryResponse getFilterCountByStoreIdAndActivatedAndViewable(String storeId, boolean activated, boolean viewable)
      throws IOException, SolrServerException;

  /**
   * Get in review products
   *
   * @param storeId
   * @param keyword
   * @param businessPartnerCode
   * @param categoryCode
   * @return
   */
  QueryResponse getInReviewProducts(String storeId, String keyword, String businessPartnerCode, String categoryCode)
      throws IOException, SolrServerException;

  /**
   * Get review products by activated and viewable flag
   *
   * @param storeId
   * @param request
   * @param activated
   * @param viewable
   * @param page
   * @param size
   * @return
   * @throws IOException
   * @throws SolrServerException
   */
  SolrDocumentList getReviewProductsByFilterRequestAndActivatedAndViewable(String storeId, SummaryFilterServiceRequest request, boolean activated,
      boolean viewable, int page, int size) throws IOException, SolrServerException;


  /**
   * Get business partners list by Filter request, activated and viewable flag
   *
   * @param storeId
   * @param request
   * @param activated
   * @param viewable
   * @param page
   * @param size
   * @return
   * @throws IOException
   * @throws SolrServerException
   */
  SolrDocumentList getBusinessPartnersByFilterRequestAndActivatedAndViewableFlag(String storeId, SummaryFilterServiceRequest request,
      boolean activated, boolean viewable, int page, int size) throws IOException, SolrServerException;

  /**
   * Get Assignee list by Filter request, activated and viewable flag
   *
   * @param storeId
   * @param request
   * @param activated
   * @param viewable
   * @return
   */
  SolrDocumentList getAssigneeListByFilterRequestAndActivatedAndViewableFlag(String storeId,
      SummaryFilterServiceRequest request, boolean activated, boolean viewable) throws IOException, SolrServerException;

  /**
   * update assigned_to field in solr on assign/un-assign action
   *
   * @param id
   * @param assignedTo
   * @throws IOException
   * @throws SolrServerException
   */
  void updateAssignedToInSolrCollection(String id, String assignedTo)
      throws CloudSolrClient.RouteException, IOException, SolrServerException;

  /**
   * update product in review product collection
   *
   * @param solrInputDocument
   * @throws Exception
   */
  void updateProductToSolrCollection(SolrInputDocument solrInputDocument) throws Exception;

  /**
   *
   * Delete all screening documents from solr before doing full reindexing
   */
  void deleteAllScreeningDocumentsFromSolr() throws IOException, SolrServerException;


  /**
   *
   * Delete all in progress documents from solr before doing full reindexing
   */
  void deleteAllInProgressDocumentsFromSolr() throws IOException, SolrServerException;

  /**
   *
   * @param dalamProductListRequest
   * @return
   */
  Page<SolrProductCollectionDTO> findDalamProductsList(DalamProductListRequest dalamProductListRequest,
      Pageable pageable) throws IOException, SolrServerException;

  /**
   * filter Product tBusinessPartner Mapper By Activated And Viewable
   * @param storeId
   * @param activated
   * @param viewable
   * @param isSearch
   * @param searchCriteria
   * @param pageable
   * @return
   */
  Page<ProductBusinessPartnerMapper> findProductBusinessPartnerMapper(String storeId, boolean activated,
      boolean viewable, boolean isSearch, String searchCriteria, Pageable pageable)
      throws IOException, SolrServerException;

  /**
   * atomic update to solr
   *
   * @param fieldsAndValues
   */
  void atomicUpdateToSolr(Map<String, Object> fieldsAndValues);
}
