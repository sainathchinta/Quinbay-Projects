package com.gdn.x.mta.distributiontask.service.api;

import com.gda.mta.product.dto.ImageQcProcessedAndBrandResponse;
import com.gda.mta.product.dto.ProductBusinessPartnerMapperResponse;
import com.gdn.x.mta.distributiontask.domain.event.model.PDTProductUpdateProductToSolrEventModel;
import com.gdn.x.mta.distributiontask.model.Product;
import com.gdn.x.mta.distributiontask.model.ProductReviewer;
import com.gdn.x.mta.distributiontask.model.dto.DistributionTaskMultipleFilterDTO;
import com.gdn.x.mta.distributiontask.model.dto.PrimaryFilterDTO;
import com.gdn.x.mta.distributiontask.model.dto.ProductAndReviewerDetailsDTO;
import com.gdn.x.mta.distributiontask.model.dto.ProductBusinessPartnerMapper;
import com.gdn.x.mta.distributiontask.model.dto.SummaryFilterDTO;
import com.gdn.x.mta.distributiontask.model.type.WorkflowState;
import com.gdn.x.mta.distributiontask.response.ProductCodeResponse;
import com.gdn.x.mta.distributiontask.rest.model.request.ProductListRequest;

import org.apache.solr.client.solrj.SolrServerException;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;

import java.io.IOException;
import java.util.Date;
import java.util.List;
import java.util.Map;

public interface SolrVendorCollectionService {

  /**
   * Full reindex pdt_product solr
   *
   * @param storeId
   */
  void fullReindexPDTProductSolr(String storeId);

  /**
   * Delta reindex pdt_product solr
   *
   * @param storeId
   */
  void deltaReindexPDTProductSolr(String storeId, String productCode);

    /**
     *
     * @param storeId
     * @param summaryFilterDTO
     * @param states
     * @param pageable
     * @return
     * @throws Exception
     */
    Page<ProductAndReviewerDetailsDTO> getVendorProductsList(String storeId, SummaryFilterDTO summaryFilterDTO,
        List<WorkflowState> states, Pageable pageable)
        throws Exception;

  /**
   * Get All product details with multiple filter from solr for Distribution list
   *
   * @param distributionTaskMultipleFilterDTO DistributionTaskMultipleFilterDTO
   * @param pageable Pageable
   * @param storeId String
   * @return Page<Product>
   * @throws Exception while fetching products from solr for distribution list
   */
  Page<Product> getAllProductDetailsWithMultipleFilterSolr(
      DistributionTaskMultipleFilterDTO distributionTaskMultipleFilterDTO, Pageable pageable,
      String storeId) throws Exception;

  /**
   *
   * @param storeId
   * @param productListRequest
   * @param states
   * @param pageable
   * @return
   * @throws Exception
   */
  Page<ProductAndReviewerDetailsDTO> getFilterProductSummary(String storeId, ProductListRequest productListRequest,
      List<WorkflowState> states, Pageable pageable) throws Exception;

  /**
   * Get vendor counts by edited and postlive flags
   *
   * @param storeId
   * @param vendorCode
   * @param edited
   * @param postLive
   * @param revised
   * @return
   */
  Map<String, Object> getFilterCounts(String storeId, String vendorCode, Boolean edited,
    Boolean postLive, Boolean revised)
      throws IOException, SolrServerException;

  /**
   * Get final Qc counts
   *
   * @param storeId
   * @return
   * @throws IOException
   * @throws SolrServerException
   */
  Map<String, Object> getFinalQcCounts(String storeId) throws IOException, SolrServerException;

  /**
   *
   * Get distribution list count
   * @param storeId
   * @param countMap
   * @return
   * @throws IOException
   * @throws SolrServerException
   */
  void getDistributionListCounts(String storeId, Map<String, Object> countMap) throws IOException, SolrServerException;

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
  List<ProductBusinessPartnerMapperResponse> getBusinessPartnerList(String storeId,
      PrimaryFilterDTO primaryFilterDTO, List<WorkflowState> states, int page, int size)
      throws IOException, SolrServerException;

  /**
   * Add product to solr
   *
   * @param product
   * @param productReviewer
   * @param imageQcProcessedAndBrandResponse
   */
  void addProductToSolr(Product product, ProductReviewer productReviewer,
    ImageQcProcessedAndBrandResponse imageQcProcessedAndBrandResponse);

  /**
   * Delete product from solr by product code
   *
   * @param productCode
   */
  void deleteProductFromSolr(String productCode) throws Exception;

  /**
   * Perform atomic update for product-vendor mapping change
   *
   * @param storeId
   * @param vendorCode
   * @param productCodeList
   */
  void assignProductToVendorAtomicUpdate(String storeId, String vendorCode,
      List<String> productCodeList);

  /**
   * Perform atomic update for reviewer changes
   *  @param productCodeList
   * @param assignedTo
   * @param assignedDate
   */
  void updateReviewerByProductCodes(List<String> productCodeList, String assignedTo,
      Date assignedDate);

  /**
   * Get review config counts by vendor code
   *
   * @param storeId
   * @param vendorCode
   * @return
   */
  Map<String, Object> getReviewConfigCountsByVendor(String storeId, String vendorCode)
      throws Exception;

  /**
   *
   * @param product
   * @throws IOException
   * @throws SolrServerException
   */
  void updateImageQcResponseToSolr(Product product, ProductReviewer productReviewer) throws IOException, SolrServerException;

  /**
   * Update solr on approval or save
   *
   * @param product
   * @param type
   */
  void updateSolrOnApprovalOrSave(Product product, ProductReviewer productReviewer, String type);

  /**
   * update solr when brand status is changed
   *
   * @param productCodes
   * @param brand
   * @param brandStatus
   */
  void updateSolrOnBrandApprovalAndRejection(List<String> productCodes, String brand, String brandStatus);

  /**
   * update solr when product is sent back to vendor
   *
   * @param product
   */
  void updateSolrOnProductSentBackToVendor(Product product) throws Exception;

  /**
   * Clear reviewer details and update state
   *
   *
   * @param productCode
   * @param state
   * @throws Exception
   */
  void clearReviewerDetailsAndUpdateState(String productCode, WorkflowState state) throws Exception;

  /**
   *
   * @param productCode
   * @param state
   * @throws Exception
   */
  void autoApprovalReviewerDetailsAndUpdateState(String productCode, WorkflowState state) throws Exception;

  /**
   * Update state on solr by atomic update
   *
   * @param productCode
   * @param stateForProduct
   */
  void updateStateOnSolr(String productCode, WorkflowState stateForProduct);

  /**
   *
   * @param productCode
   * @param stateForProduct
   */
  void updateStateAndMfdTrueOnSolr(String productCode, WorkflowState stateForProduct);

  /**
   *
   * @param productCode
   * @param postLive
   */
  void updatePostLiveFlag(String productCode, boolean postLive);

  /**
   * Get review config counts by vendor code and configuration
   *
   * @param storeId
   * @param vendorCode
   * @param postLive
   * @return
   */
  Map<String, Object> getReviewConfigCountsByVendorAndConfig(String storeId, String vendorCode, boolean postLive)
      throws Exception;

  /**
   * Get business Partner list by workflows
   *
   * @param workflowStateList
   * @param searchCriteria
   * @param pageable
   * @param storeId
   * @return
   */
  Page<ProductBusinessPartnerMapper> findProductBusinessPartnerMapper(List<WorkflowState> workflowStateList,
      String searchCriteria, Pageable pageable, String storeId) throws Exception;

  /**
   *
   * @param product
   */
  void updateSolrOnApprovalOrSave(PDTProductUpdateProductToSolrEventModel product) throws Exception;

  /**
   * Update pdt solr on retry status update
   *
   * @param updatedProduct
   * @throws Exception
   */
  void updateProductOnRetryStatusUpdate(Product updatedProduct) throws Exception;

  /**
   * publish solr add pdt product event in batches
   * @param productAndReviewerDetailsDTOList
   * @throws Exception
   */
  public void publishSolrAddPDTProductBatchEvent(List<ProductAndReviewerDetailsDTO> productAndReviewerDetailsDTOList) throws Exception;


  /**
   * Get Product Response For Auto Assignment in batches
   * @param summaryFilterDTO
   * @param storeId
   * @param pageable
   * @throws Exception
   */
  Page<ProductCodeResponse> getProductResponseForAutoAssignment(String storeId, SummaryFilterDTO summaryFilterDTO,
    Pageable pageable) throws SolrServerException, IOException;
}
