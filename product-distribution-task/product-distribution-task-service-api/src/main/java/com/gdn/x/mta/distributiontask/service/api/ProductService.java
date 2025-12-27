package com.gdn.x.mta.distributiontask.service.api;


import java.io.IOException;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.Set;

import com.gdn.partners.pdt.dto.configuration.distribution.ApproveProductResponseDto;
import com.gdn.x.mta.distributiontask.domain.event.InternalHistoryEventModel;
import com.gdn.x.mta.distributiontask.model.dto.ProductAndReviewerDetailsDTO;
import com.gdn.x.mta.distributiontask.request.AppealProductRequest;
import com.gdn.x.mta.distributiontask.request.ProductRetryStatusUpdate;
import com.gdn.x.mta.distributiontask.response.ProductCodeResponse;
import com.gdn.x.mta.distributiontask.rest.model.request.BoostedProductFilterRequest;
import com.gdn.x.mta.distributiontask.rest.model.request.ChangeBrandRequest;
import com.gdn.x.mta.distributiontask.rest.model.request.PublishAndSavedProductAndHistoryModel;
import com.gdn.x.mta.distributiontask.rest.model.response.AppealProductResponse;
import com.gdn.x.mta.distributiontask.rest.model.response.VendorQuickApprovalResponse;

import org.apache.commons.lang3.tuple.Pair;
import org.apache.solr.client.solrj.SolrServerException;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Slice;

import com.gda.mta.product.dto.ProductBusinessPartnerMapperResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.mta.domain.event.modal.ImageQcProcessedResponseDomainEvent;
import com.gdn.mta.domain.event.modal.PDTDimensionRefreshEventModel;
import com.gdn.x.mta.distributiontask.model.Product;
import com.gdn.x.mta.distributiontask.model.ProductActionRetry;
import com.gdn.x.mta.distributiontask.model.ProductAutoApproval;
import com.gdn.x.mta.distributiontask.model.ProductReviewer;
import com.gdn.x.mta.distributiontask.model.Vendor;
import com.gdn.x.mta.distributiontask.model.dto.BulkScreeningProductActionsDTO;
import com.gdn.x.mta.distributiontask.model.dto.DistributionTaskMultipleFilterDTO;
import com.gdn.x.mta.distributiontask.model.dto.NeedRevisionDTO;
import com.gdn.x.mta.distributiontask.model.dto.PrimaryFilterDTO;
import com.gdn.x.mta.distributiontask.model.dto.ProductBusinessPartnerMapper;
import com.gdn.x.mta.distributiontask.model.dto.RejectProductDTO;
import com.gdn.x.mta.distributiontask.model.dto.SummaryFilterDTO;
import com.gdn.x.mta.distributiontask.model.dto.VendorProductStatusDTO;
import com.gdn.x.mta.distributiontask.model.enums.AutoApprovalStatus;
import com.gdn.x.mta.distributiontask.model.type.WorkflowState;
import com.gdn.x.mta.distributiontask.request.NeedRevisionRequest;
import com.gdn.x.mta.distributiontask.rest.model.request.AutoNeedRevisionRequest;
import com.gdn.x.mta.distributiontask.rest.model.response.NeedRevisionResponse;
import com.gdn.x.mta.distributiontask.rest.model.response.QuickApprovalResponse;
import com.gdn.x.productcategorybase.domain.event.model.BrandApprovedOrRejectedDomainEventModel;
import com.gdn.x.productcategorybase.domain.event.model.ImagePathUpdateDomainEventModel;

public interface ProductService {
  /**
   * Create new product from product screening approval subscriber
   *
   * @param product
   * @return
   */
  Product createProduct(Product product);

  /**
   * This method is the verify whether merchant has modified product or not while vendor verification.
   *
   * @param productCode
   * @param version
   * @return
   */
  boolean getEditedByMerchant(String productCode, long version);

  /**
   * Service method to count all product in distribution task with filter
   *
   * @param includeStatus
   * @param includeVendors
   * @param distributionTaskMultipleFilterDTO @return
   * @throws Exception
   */
  Map<String, Object> countAllProductDetailsWithMultipleFilter(Boolean includeStatus, Boolean includeVendors,
                                                               DistributionTaskMultipleFilterDTO
                                                                   distributionTaskMultipleFilterDTO) throws Exception;

  /**
   * To fetch Product by productID
   *
   * @param productId
   * @return
   */
  public Product findByProductId(String productId) throws Exception;

  /**
   * get all product details by product Code @param code @return @throws
   */
  Product getAllProductDetailsByCode(String code) throws Exception;

  /**
   *
   * Auto heal product data
   *
   * @param product
   * @param type
   * @return
   * @throws Exception
   */
  Product autoHealProductData(Product product, String type) throws Exception;

  /**
   * Auto heal product distribution task
   *
   * @param product
   */
  void autoHealProductDistributionTask(Product product) throws Exception;

  /**
   * Product And Item Image Path Update
   *
   * @param request
   * @throws Exception
   */
  void productAndItemImagePathUpdate(ImagePathUpdateDomainEventModel request);


  /**
   * Vendor product approval
   *
   * @param product
   * @param vendorCode
   * @param notes
   * @param isQuickApproval
   * @param productReviewer
   * @param isBulkAction
   * @return
   * @throws Exception
   */
  ApproveProductResponseDto approveProductByVendor(Product product, String vendorCode, String notes,
    boolean isQuickApproval, ProductReviewer productReviewer, boolean isBulkAction)
    throws Exception;

  /**
   * reject product by Vendor
   *
   * @param rejectProductDTO
   * @param vendorCode
   * @throws Exception
   */
  void rejectProductByVendor(RejectProductDTO rejectProductDTO, String vendorCode) throws Exception;

  void rejectProduct(RejectProductDTO rejectProductDTO) throws Exception;

  /**
   * get product by code and mark for delete false
   *
   * @param code
   * @return
   */
  Product getProductByCode(String code);

  /**
   *
   * @param productCode
   * @return
   */
  Product getProductByProductCodeAndMarkForDeleteFalse(String productCode);

  /**
   *
   * @param code
   * @return
   */
  Product getDetailsForProductByProductCodeAndMarkForDeleteFalse(String code);

  /**
   * Get product details by productCode
   *
   * @param code
   * @return
   */
  Product getDetailsForProductByProductCode(String code);


  /**
   *
   *
   * @param code
   * @return
   */
  Product findProductByProductCode(String code);

  /**
   * Get product and items by code
   *
   * @param productCode
   * @return
   */
  Product getProductByProductCodeAndMarkForDeleteFalseAndItems(String productCode);

  /**
   * Do product neeed revision
   *
   * @param vendorCode
   * @param productCode
   * @param request
   * @return
   * @throws Exception
   */
  NeedRevisionDTO doProductNeedForCorrectionSync(String vendorCode, String productCode, NeedRevisionRequest request) throws Exception;

  /**
   * To Fetch Product List Based On Product Codes
   *
   * @param productCodeList
   * @return
   */
  List<Product> getProductListByProductCodes(List<String> productCodeList, List<WorkflowState> states);

  /**
   * get vendor Id from vendor Code
   *
   * @param vendorCode
   * @return
   * @throws Exception
   */
  String getvendorIdByVendorCode(String vendorCode) throws Exception;

  /**
   * Find pending products in vendor
   *
   * @param storeId
   * @param vendorCode
   * @param postLive
   * @param edited
   * @param revised
   * @return
   */
  Map<String, Object> getProductStatusByVendor(String storeId, String vendorCode, Boolean postLive,
    Boolean edited, Boolean revised) throws Exception;

  /**
   *
   * @param storeId
   * @param vendorCode
   * @return
   */
  Map<String, Object> getReviewConfigProductCountByVendor(String storeId, String vendorCode)
      throws Exception;

  /**
   * Service method to retrieve workflowStatus Map for all products related to given list of product
   * ids
   *
   * @param productIdList
   * @return
   * @throws Exception
   */
  Map<String, List<WorkflowState>> getWorkflowStatusForProducts(List<String> productIdList)
      throws Exception;


  /**
   * Service method to retrieve workflowStatus Map for all products related to given list of product
   * ids and vendor code
   *
   * @param vendorId
   * @param productIdList
   * @return
   * @throws Exception
   */
  Map<String, List<WorkflowState>> getWorkflowStatusForProducts(String vendorId,
      List<String> productIdList) throws Exception;

  /**
   * To reject discard product and then publish the product code that has been discarded, so the
   * edited product wil be replaced with the original one
   *
   * @param productId
   * @return
   * @throws Exception
   */
  void rejectAndDiscardProduct(String productId, boolean isExceededSla) throws Exception;

  /**
   * Get product details for any product type - including markForDelete true
   *
   * @param code
   * @return
   * @throws Exception
   */
  Product getDetailsForAnyProductTypeByCode(String code) throws Exception;

  /**
   * remove product details by setting markForDelete to true
   *
   * @param product
   * @param approverAssignee
   * @throws Exception
   */
  void removeProductWithMarkForDelete(Product product, String approverAssignee) throws Exception;

  /**
   * To replace discarded product with original product details from subscriber
   *
   * @param product
   * @return
   */
  Product replaceProduct(Product product);


  /**
   * update Product details @param product @throws
   */
  void updateProduct(Product product) throws Exception;


  /**
   * Update Product Details by Vendor
   *
   * @param existingProduct
   * @param newProduct
   * @param deleteOriginalImages
   * @return
   * @throws Exception
   */
  Product updateProductDetails(Product existingProduct,
      Product newProduct,
      boolean deleteOriginalImages) throws Exception;

  /**
   * Update Product Details by Vendor
   *
   * @param existingProduct
   * @param newProduct
   * @return
   * @throws Exception
   */
  Product updateProductDetails(Product existingProduct, Product newProduct) throws Exception;

  /**
   * Update Product Details by Vendor
   *
   * @param existingProduct
   * @param newProduct
   *
   * @throws Exception
   */
  Product updateEditedProductDetails(Product existingProduct, Product newProduct, List<String> modifiedFields) throws Exception;

  /**
   *
   * @param product
   * @param modifiedFields
   * @throws IOException
   */
  void updateProductNotesForEditedProducts(Product product, List<String> modifiedFields) throws IOException;

  /**
   * update product image related details
   *
   * @param existingProduct
   * @param newProduct
   * @throws Exception
   */
  Product updateProductImageDetails(Product existingProduct, Product newProduct, boolean deleteOriginalImages) throws Exception;

  /**
   * update product image related details
   *
   * @param existingProduct
   * @param newProduct
   * @throws Exception
   */
  Product updateEditedProductImageDetails(Product existingProduct, Product newProduct) throws Exception;

  /**
   * update Product
   *
   * @param product
   * @return
   */
  Product update(Product product);

  /**
   * Update product state
   *
   * @param product
   * @param state
   * @return
   */
  Product updateState(Product product, WorkflowState state);

  /**
   * Update product state and assignee details
   *
   * @param product
   * @param state
   * @return
   */
  Product updateStateAndRemoveAssigneeDetails(Product product, WorkflowState state);

  /**
   * save products in bulk
   *
   * @param productList
   * @throws Exception
   */
  void saveBulkProducts(List<Product> productList) throws Exception;

  /**
   * To get List of Business Partners based on workflowState
   * @param workflowState
   * @param searchCriteria
   * @param pageable
   * @param isSearch
   * @param storeId
   * @return
   */
  Page<ProductBusinessPartnerMapper> findProductBusinessPartnerMapper(WorkflowState workflowState,
      String searchCriteria, Pageable pageable, boolean isSearch, String storeId) throws Exception;

  /**
   * To get count of all the status based on vendor
   * @param vendor
   * @param storeId
   * @return
   */
  List<VendorProductStatusDTO> findProductStatusForVendor(Vendor vendor, String storeId);

  /**
   * To get all Business partner for vendor, mapped with product(s) for vendor
   *
   * @param vendorId
   * @param pageable
   * @return
   */
  Page<ProductBusinessPartnerMapper> getBusinessPartnerForVendor(String vendorId,
      Pageable pageable);

  /**
   * multi filter applies to get product list in distribution page
   *
   * @param distributionTaskMultipleFilterDTO DistributionTaskMultipleFilterDTO
   * @param pageable Pageable
   * @param storeId String
   * @return Page<Product>
   * @throws Exception while fetching distribution list
   */
  Page<Product> getAllProductDetailsWithMultipleFilter(
      DistributionTaskMultipleFilterDTO distributionTaskMultipleFilterDTO, Pageable pageable,
      String storeId) throws Exception;

  /**
   * get existing product codes from PDT
   *
   * @param productCodes
   * @return
   * @throws Exception
   */
  List<String> getProductCodeList(List<String> productCodes) throws Exception;

  /**
   * Retrieves business partners according to primary filter
   *
   * @param storeId
   * @param requestId
   * @param request
   * @param page
   * @param size
   * @return
   */
  GdnRestListResponse<ProductBusinessPartnerMapperResponse> getBusinessPartnerList(String storeId, String requestId,
      PrimaryFilterDTO request, int page, int size) throws IOException, SolrServerException;

  /**
   * Fetches list of assignees based on primary filter
   *
   * @param storeId
   * @param requestId
   * @param request
   * @return
   */
  List<String> getAssigneeList(String storeId, String requestId, PrimaryFilterDTO request);

  /**
   * Fetches product list based on various filters
   *
   * @param storeId
   * @param requestId
   * @param username
   * @param summaryFilterDTO
   * @param page
   * @param size
   * @return
   */
  Page<ProductAndReviewerDetailsDTO> getProductList(String storeId, String requestId, String username, SummaryFilterDTO summaryFilterDTO,
      int page, int size) throws Exception;

  /**
   *
   * @param productList
   */
  void setVendorForProductList(List<Product> productList);

  /**
   * Updates the assignee for the vendor product.
   *
   * @param storeId
   * @param productCodes
   * @param assignedBy
   * @param action
   * @param date
   * @return
   */
  List<String> doVendorProductAction(String storeId, List<String> productCodes, String assignedBy, String action,
      String assignedTo, Date date) throws Exception;

  /**
   * Bulk vendor product action assignment
   *
   * @param storeId
   * @param date
   * @param bulkScreeningProductActionsDTO
   * @throws Exception
   */
  void bulkVendorProductAction(String storeId, Date date, BulkScreeningProductActionsDTO bulkScreeningProductActionsDTO)
      throws Exception;

  /**
   *
   * @param vendorCode
   * @param newProduct
   * @param notes
   * @param isQuickApproval
   * @param productReviewer
   * @return
   * @throws Exception
   */
  ApproveProductResponseDto updateAndApproveProduct(String vendorCode, Product newProduct,
    String notes, boolean isQuickApproval, ProductReviewer productReviewer) throws Exception;

  /**
   * Do vendor product need correction
   *
   * @param storeId
   * @param requestId
   * @param username
   * @param vendorCode
   * @param request
   * @throws Exception
   * @return
   */
  NeedRevisionResponse doProductNeedForCorrection(String storeId, String requestId, String username, String vendorCode,
    NeedRevisionRequest request) throws Exception;

  /**
   * Find products by mark for delete true before the updated date
   */
  Slice<Object[]> findAllProductIdsBeforeUpdatedDateAndMarkForDeleteTrue(Date updatedDate, Pageable pageable);

  /**
   * Get complete product
   *
   * @param updatedDate
   * @param pageable
   * @return
   */
  Slice<Product> findProductIdsBeforeUpdatedDateAndMarkForDeleteTrueAndPickedForDeleteFalse(Date updatedDate, Pageable pageable);

  /**
   * Deletes the products from database which are last updated before the given days.
   *
   */
  void deleteProducts(String storeId, List<String> productIds, List<String> productCodes, int batchSize) throws Exception;

  /**
   * Send product back to vendor
   *
   * @param productCode
   * @return
   * @throws Exception
   */
  Product sendProductBackToVendor(String productCode) throws Exception;

  /**
   * Republish product activation event for products in qc state even after current time - delta time
   * @param qcRetryCount
   * @param deltaHours
   * @param batchSize
   */
  List<Product> republishFinalQcProductsForApproval(int qcRetryCount, int deltaHours, int batchSize);

  /**
   * to retry final qc products manually
   *
   * @param productCode
   */
  void retryFinalQCProducts(String productCode);

  /**
   *
   * @param storeId
   * @param states
   * @param markForDelete
   * @param postLive
   * @param date
   * @param pageSize
   * @return
   */
  List<Product> getUnassignedProductsByStoreIdAndStateAndMarkForDeleteAndPostLive(
      String storeId, List<WorkflowState> states, boolean markForDelete, boolean postLive, Date date, int pageSize);

  /**
   *
   * @param productCode
   * @throws Exception
   */
  void updateProductAsPostLiveTrue(String productCode) throws Exception;


  /**
   * Delete original images from storage source for products and items
   *
   * @param product
   */
  void deleteOriginalImagesForProductAndItems(Product product);

  /**
   * update image qc response
   *
   * @param imageQcProcessedResponseDomainEvent
   * @param product
   * @return pair of product and publishEvent boolean
   * @throws Exception
   */
  PublishAndSavedProductAndHistoryModel updateImageQcResponseByProductCode(
      ImageQcProcessedResponseDomainEvent imageQcProcessedResponseDomainEvent, Product product) throws Exception;


  /**
   * Publish events
   *
   * @param doPublishEvent
   * @param product
   */
  void publishAutoApprovalEvents(boolean doPublishEvent, Product product);

  /**
   * Update brand approval status
   *
   * @param brandApprovedOrRejectedDomainEventModel
   * @return
   */
  List<String> updateBrandApprovalStatus(BrandApprovedOrRejectedDomainEventModel brandApprovedOrRejectedDomainEventModel);

  /**
   * Fetch all products with mfd false from pdt_product table
   *
   * @param storeId
   * @param pageable
   * @return
   */
  Page<Product> findByStoreIdAndMarkForDeleteFalseOrderByCreatedDateDesc(String storeId, Pageable pageable);

  /**
   * Get updated products for delta indexing the collection
   *
   * @param storeId
   * @param startDate
   * @param endDate
   * @param pageable
   * @return
   */
  Page<Product> findByStoreIdAndUpdatedDateBetweenOrderByProductCodeAscAndUpdatedDateAsc(String storeId, Date startDate,
      Date endDate, Pageable pageable);

  /**
   *
   * @param storeId
   * @param vendorCode
   * @param postLive
   * @return
   */
  Map<String, Object> getReviewConfigProductCountByVendorAndConfig(String storeId, String vendorCode, boolean postLive)
      throws Exception;

  /**
   * Update approved product data
   *
   * @param existingProduct
   * @param newProduct
   */
  void updateApprovedProductData(Product existingProduct, Product newProduct);

  /**
   * Update revised product data
   *  @param existingProduct
   * @param newProduct
   */
  Product updateRevisedProductData(Product existingProduct, Product newProduct, List<String> modifiedFields)
      throws IOException;

  /**
   *
   * @param existingProduct
   * @param modifiedFields
   * @throws IOException
   */
  void updateProductNotesForRevisedProducts(Product existingProduct, List<String> modifiedFields)
      throws IOException;

  /**
   * Republish edited product activation event
   *
   * @param productCode
   */
  void republishEditedProduct(String productCode);

  /**
   * @param storeId
   * @param productAutoApprovalList
   * @param maxNumberOfDaysToApproveAssigneeProducts
   */
  void autoApprovePendingProducts(String storeId,
      List<ProductAutoApproval> productAutoApprovalList, int maxNumberOfDaysToApproveAssigneeProducts);

  /**
   * @param product
   * @param isEligibleForAutoApproval
   * @param maxNumberOfDaysToApproveAssignedProducts
   * @return
   * @throws Exception
   */
  Pair<AutoApprovalStatus, InternalHistoryEventModel> autoApproveOfPendingProductsAfterEligibilityCheck(Product product
      , boolean isEligibleForAutoApproval, int maxNumberOfDaysToApproveAssignedProducts) throws Exception;

  /**
   *
   * @param storeId
   * @param autoNeedRevisionRequest
   * @return
   * @throws Exception
   */
  Product updateProductToAutoNeedRevision(String storeId, AutoNeedRevisionRequest autoNeedRevisionRequest,
      boolean validateAssignment) throws Exception;

  /**
   * Update product task and history
   *
   * @param product
   * @param autoNeedRevisionRequest
   */
  void updateProductTaskAndHistory(Product product, AutoNeedRevisionRequest autoNeedRevisionRequest);

  /**
   *
   * @param storeId
   * @param productActionRetryList
   */
  void autoNeedReviseForPendingProducts(String storeId, List<ProductActionRetry> productActionRetryList);

  /**
   * Quick approval of product
   *
   * @param vendorCode
   * @param productCode
   * @param notes
   * @param productReviewer
   * @param isBulkAction
   * @param product
   * @return
   * @throws Exception
   */
  QuickApprovalResponse quickApproveProduct(String vendorCode, String productCode, String notes,
      ProductReviewer productReviewer, boolean isBulkAction, Product product) throws Exception;

  /**
   * validates and heals product data before allowing quick apprioval
   *
   * @param productCode product code
   * @param product product data to be healed
   * @param vendorQuickApprovalResponse cannot be null
   * @return healed product
   * @throws Exception e
   */
  Product autoHealProductDataForApprovalIfEligible(String productCode, Product product,
    VendorQuickApprovalResponse vendorQuickApprovalResponse) throws Exception;

  /**
   * Refresh product dimensions, product type, dg level
   * @param pdtDimensionRefreshEventModel
   */
  void updateProductDimensionsAndProductTypeAndDgLevel(PDTDimensionRefreshEventModel pdtDimensionRefreshEventModel)
      throws IOException;

  /**
   * Auto approve product
   *
   * @param productCode
   * @return
   */
  PublishAndSavedProductAndHistoryModel autoApproveProduct(String productCode) throws Exception;

  /**
   * @param productCodes
   * @return
   */
  List<Product> getProductsByProductCodes(List<String> productCodes);

  /**
   * fetch batch of products by seller code and category code
   * @param storeId
   * @param sellerCode
   * @param state
   * @param categoryCodes
   * @param pageable
   * @return
   */
  Page<Product> getProductsBySellerCodeAndCategoryCodes(String storeId, String sellerCode, WorkflowState state,
      Set<String> categoryCodes, Pageable pageable);

  /**
   * Update product status for retry
   *
   * @param storeId
   * @param productCode
   * @param productRetryStatusUpdate
   * @throws Exception
   */
  void updateProductRetryStatus(String storeId, String productCode, ProductRetryStatusUpdate productRetryStatusUpdate)
    throws Exception;

  /**
   * filter Product With  Boost For AutoAssignment
   *
   * @param storeId
   * @param boostedProductFilterRequest
   * @param page
   * @param size
   * @param username
   * @param requestId
   * @throws Exception
   */
  Page<ProductCodeResponse> filterProductWithBoostForAutoAssignment(String storeId,
    String requestId, String username, BoostedProductFilterRequest boostedProductFilterRequest, Integer page, Integer size)
    throws SolrServerException, IOException;

  /**
   * Check if product code already auto heal once
   *
   * @param keyword
   * @return
   */
  String checkIfVendorAutoHealKeyExists(String keyword);


  /**
   * Cache vendor auto heal search keyword
   *
   * @param keyword
   * @return
   */
  String cacheVendorAutoHealKey(String keyword);

  /**
   * Update brand of a product
   * @param changeBrandRequest
   */
  Product updateBrandOfProduct(ChangeBrandRequest changeBrandRequest) throws Exception;

  /**
   * @param storeId
   * @param startUpdatedDate
   * @param endUpdatedDate
   * @param pageable
   */
  Page<Product> fetchNeedCorrectionProducts(String storeId, Date startUpdatedDate, Date endUpdatedDate,
      Pageable pageable);

  /**
   * publish internal history event for product
   *
   * @param internalHistoryEventModel internalHistoryEventModel
   */
  void publishInternalHistoryEventForProduct(InternalHistoryEventModel internalHistoryEventModel);

  /**
   * to update product and solr for appealed product
   *
   * @param appealProductRequest
   * @param storeId
   */
  AppealProductResponse updateAppealProduct(AppealProductRequest appealProductRequest,
    String storeId) throws Exception;
}

