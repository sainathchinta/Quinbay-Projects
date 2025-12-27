package com.gdn.x.mta.distributiontask.dao.api;

import java.io.IOException;
import java.util.List;

import org.apache.solr.client.solrj.SolrServerException;
import org.apache.solr.client.solrj.response.QueryResponse;
import org.apache.solr.common.SolrDocument;
import org.apache.solr.common.SolrException;
import org.apache.solr.common.SolrInputDocument;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;

import com.gdn.x.mta.distributiontask.domain.event.model.PDTProductUpdateProductToSolrEventModel;
import com.gdn.x.mta.distributiontask.model.Product;
import com.gdn.x.mta.distributiontask.model.ProductReviewer;
import com.gdn.x.mta.distributiontask.model.dto.DistributionTaskMultipleFilterDTO;
import com.gdn.x.mta.distributiontask.model.dto.PrimaryFilterDTO;
import com.gdn.x.mta.distributiontask.model.dto.SummaryFilterDTO;
import com.gdn.x.mta.distributiontask.model.solr.VendorProductSolr;
import com.gdn.x.mta.distributiontask.model.type.WorkflowState;
import com.gdn.x.mta.distributiontask.rest.model.request.ProductListRequest;

public interface SolrVendorProductCollectionRepository {

    /**
     * Delete solr documents by productCode or deleteAll if productCode is empty & flag set
     *
     * @param productCodes
     * @param deleteAll
     * @throws Exception
     */
    void deleteDocumentFromSolr(List<String> productCodes, boolean deleteAll) throws Exception;

    /**
     * Add list of solr documents
     *
     * @param vendorProductSolr
     */
    void addDocumentToSolrWithCategoryHierarchy(List<VendorProductSolr> vendorProductSolr);

    /**
     *
     * @param storeId
     * @param summaryFilterDTO
     * @param states
     * @param pageable
     * @return
     * @throws Exception
     */
    Page<VendorProductSolr> getVendorProductsBySolrQuery(String storeId,
        SummaryFilterDTO summaryFilterDTO, List<WorkflowState> states, Pageable pageable) throws Exception;

  /**
   * Get product details by multiple filters for Distribution list
   *
   * @param distributionTaskMultipleFilterDTO
   * @param pageable
   * @return
   * @throws IOException
   * @throws SolrServerException
   * @throws SolrException
   */
  Page<VendorProductSolr> getAllProductDetailsWithMultipleFilterForDistributionListBySolrQuery(
      DistributionTaskMultipleFilterDTO distributionTaskMultipleFilterDTO, Pageable pageable)
      throws IOException, SolrServerException, SolrException;

  /**
   *
   * @param storeId
   * @param productListRequest
   * @param states
   * @param pageable
   * @return
   * @throws Exception
   */
  Page<VendorProductSolr> getFilterProductSummaryBySolrQuery(String storeId,
      ProductListRequest productListRequest, List<WorkflowState> states, Pageable pageable) throws Exception;


  /**
   * Get vendor counts by edited and postLive flags
   *
   * @param storeId
   * @param vendorCode
   * @param edited
   * @param postLive
   * @param revised
   * @return
   * @throws IOException
   * @throws SolrServerException
   */
  QueryResponse getFilterCounts(String storeId, String vendorCode, Boolean edited, Boolean postLive,
    Boolean revised)
      throws IOException, SolrServerException;

  /**
   * Get final qc count
   *
   * @param storeId
   * @return
   */
  QueryResponse getFinalQCCounts(String storeId) throws IOException, SolrServerException;

  /**
   * Get final qc count
   *
   * @param storeId
   * @return
   */
  QueryResponse getDistributionListCounts(String storeId) throws IOException, SolrServerException;

  /**
   * Atomic update by single document
   *
   * @param solrInputDocument
   */
  void executeAtomicUpdate(SolrInputDocument solrInputDocument);

  /**
   * Atomic update by list of documents
   *
   * @param solrInputDocumentList
   */
  void executeAtomicUpdateForListOfInputDocuments(List<SolrInputDocument> solrInputDocumentList);

  /**
   * Get business partner list
   *
   * @param storeId
   * @param primaryFilterDTO
   * @param states
   * @return
   * @throws IOException
   * @throws SolrServerException
   */
  QueryResponse getBusinessPartnerList(String storeId, PrimaryFilterDTO primaryFilterDTO, List<WorkflowState> states, int page, int size)
      throws IOException, SolrServerException;

  /**
   * Fetch review config counts by vendor code
   *
   * @param storeId
   * @param vendorCode
   * @return
   */
  QueryResponse getReviewConfigCountsByVendor(String storeId, String vendorCode)
      throws IOException, SolrServerException, SolrException;

  /**
   * Update solr on content approval
   *
   * @param product
   * @param categoryChanged
   */
  void updateSolrOnContentApprovalOrSave(Product product, ProductReviewer productReviewer, boolean categoryChanged) throws IOException, SolrServerException;

  /**
   *
   * @param product
   * @param categoryChanged
   * @throws IOException
   * @throws SolrServerException
   */
  void updateSolrOnApprovalOrSave(PDTProductUpdateProductToSolrEventModel product, boolean categoryChanged) throws IOException, SolrServerException;

  /**
   * Update solr on image approval
   *
   * @param product
   */
  void updateSolrOnImageApproval(Product product, ProductReviewer productReviewer) throws IOException, SolrServerException;

  /**
   * Get solr document by product code
   *
   * @param productCode
   * @return
   */
  SolrDocument getSolrDocument(String productCode) throws IOException, SolrServerException;

  /**
   * Update solr on brand approval or rejection
   *
   * @param productCode
   * @param brand
   * @param brandStatus
   */
  void updateSolrOnBrandApprovalOrRejection(String productCode, String brand, String brandStatus)
      throws IOException, SolrServerException;

  /**
   * Get review config counts by vendor code and configuration
   *
   * @param storeId
   * @param vendorCode
   * @return
   */
  QueryResponse getReviewConfigCountsByVendorAndConfig(String storeId, String vendorCode, boolean postLive)
      throws IOException, SolrServerException, SolrException;

  /**
   * Get business Partner list by workflows
   *
   * @param workflowStateList
   * @param searchCriteria
   * @param pageable
   * @param storeId
   * @return
   */
  QueryResponse findProductBusinessPartnerMapper(List<WorkflowState> workflowStateList, String searchCriteria,
      Pageable pageable, String storeId) throws IOException, SolrServerException;


  /**
   * Get Filtered And Boosted Products From Solr
   *
   * @param summaryFilterDTO
   * @param pageable
   * @param storeId
   * @throws
   * @return
   */
  Page<VendorProductSolr> getFilteredAndBoostedProductsFromSolr(String storeId,
    SummaryFilterDTO summaryFilterDTO, Pageable pageable) throws SolrServerException, IOException;
}
