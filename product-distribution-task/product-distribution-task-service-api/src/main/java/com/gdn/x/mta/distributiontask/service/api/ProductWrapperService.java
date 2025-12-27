package com.gdn.x.mta.distributiontask.service.api;

import java.util.List;

import com.gda.mta.product.dto.response.AutoApprovalTypeResponse;
import com.gdn.mta.domain.event.modal.ImageQcProcessedResponseDomainEvent;
import com.gdn.x.mta.distributiontask.domain.event.model.ProductChange;
import com.gdn.x.mta.distributiontask.model.Product;
import com.gdn.x.mta.distributiontask.model.ProductActionRetry;
import com.gdn.x.mta.distributiontask.model.dto.BulkVendorProductActionsDTO;
import com.gdn.x.mta.distributiontask.model.dto.CategoryDTO;
import com.gdn.x.mta.distributiontask.model.dto.RejectProductDTO;
import com.gdn.x.mta.distributiontask.request.NeedRevisionRequest;
import com.gdn.x.mta.distributiontask.request.RemoveProductRequest;
import com.gdn.x.mta.distributiontask.rest.model.request.AutoNeedRevisionRequest;
import com.gdn.x.mta.distributiontask.rest.model.request.ChangeBrandRequest;
import com.gdn.x.mta.distributiontask.rest.model.request.VendorQuickApprovalRequest;
import com.gdn.x.mta.distributiontask.rest.model.response.BulkVendorProductActionsResponse;
import com.gdn.x.mta.distributiontask.rest.model.response.NeedRevisionResponse;
import com.gdn.x.mta.distributiontask.rest.model.response.VendorQuickApprovalResponse;
import com.gdn.x.productcategorybase.domain.event.model.BrandApprovedOrRejectedDomainEventModel;

public interface ProductWrapperService {

  /**
   *
   * @param product
   * @param vendorCode
   * @param notes
   * @throws Exception
   */
  void updateAndApproveProduct(Product product, String vendorCode, String notes) throws Exception;

  /**
   * Update product details
   *
   * @param existingProduct
   * @param newProduct
   */
  Product updateProductDetails(Product existingProduct, Product newProduct) throws Exception;

  /**
   * Update product details
   *
   * @param existingProduct
   * @param newProduct
   */
  Product updateEditedProductDetails(Product existingProduct, Product newProduct, List<String> modifiedFields) throws Exception;

  /**
   * Update image QC response
   *
   * @param imageQcProcessedResponseDomainEvent
   */
  void updateImageQcResponseByProductCode(ImageQcProcessedResponseDomainEvent imageQcProcessedResponseDomainEvent)
      throws Exception;

  /**
   * Update brand status for products
   *
   * @param brandApprovedOrRejectedDomainEventModel
   * @throws
   */
  void updateBrandApprovalStatusAndUpdateSolr(
      BrandApprovedOrRejectedDomainEventModel brandApprovedOrRejectedDomainEventModel) throws Exception;

  /**
   * Delete wip products after receiving deletion event
   *
   * @param productCode
   * @param notes
   */
  void deleteProductWipAndReindexSolr(String productCode, String notes) throws Exception;

  /**
   * Remove product and delete original images
   *
   * @param removeProductRequest
   */
  void removeProductAndDeleteOriginalImages(RemoveProductRequest removeProductRequest) throws Exception;

  /**
   * Remove product on need correction action
   *
   *
   * @param storeId
   * @param requestId
   * @param username
   * @param vendorCode
   * @param request
   * @throws Exception
   * @return
   */
  NeedRevisionResponse doProductNeedForCorrectionAndReindexSolr(String storeId, String requestId, String username, String vendorCode,
    NeedRevisionRequest request) throws Exception;

  /**
   * Send product back to vendor review by product code
   *
   * @param productCode
   */
  void sendProductBackToVendorAndReindexSolr(String productCode) throws Exception;

  /**
   * Reject product and remove from solr
   *
   * @param rejectProductDTO
   * @param vendorCode
   * @throws Exception
   */
  void rejectProductByVendorAndDeleteFromSolr(RejectProductDTO rejectProductDTO, String vendorCode) throws Exception;

  /**
   * Performing rejection for products having definitive actions as auto reject
   *
   * @param productActionRetryList
   */
  void pdtAutoRejectForPendingProducts(List<ProductActionRetry> productActionRetryList);

  /**
   * Bulk assign products
   *
   * @param storeId
   * @param bulkVendorProductActionsDTO
   * @return
   * @throws Exception
   */
  BulkVendorProductActionsResponse bulkUpdateProductAssignee(String storeId,
      BulkVendorProductActionsDTO bulkVendorProductActionsDTO) throws Exception;

  /**
   * Update content and image for list of productCodes
   * @param storeId
   * @param productCodes
   * @param assignedBy
   * @param action
   * @param assignedTo
   */
  void updateAssigneeDetails(String storeId, List<String> productCodes, String assignedBy, String action,
      String assignedTo) throws Exception;

  /**
   * @param storeId
   * @param productCode
   * @param isEligibleForAutoApproval
   * @param autoApprovalTypeResponse
   */
  void autoApproveOfPendingProductsAfterEligibilityCheck(String storeId,
      String productCode, boolean isEligibleForAutoApproval, AutoApprovalTypeResponse autoApprovalTypeResponse);

  /**
   *
   * @param storeId
   * @param autoNeedRevisionRequest
   * @throws Exception
   */
  void updateProductToAutoNeedRevision(String storeId, AutoNeedRevisionRequest autoNeedRevisionRequest,
      boolean validateAssignment) throws Exception;

  /**
   * Product quick approval by the vendor
   * @param vendorQuickApprovalRequest
   */
  VendorQuickApprovalResponse quickApproveProduct(VendorQuickApprovalRequest vendorQuickApprovalRequest)
      throws Exception;

  /**
   * backfill commonImage flag in pdt product images and item images
   * @param storeId
   * @param productCode
   */
  void backfillCommonImageFlagInProductAndItemImages(String storeId, String productCode);


  /**
   * Get product details by productCode and mark for delete false
   *
   * @param code
   * @return
   * @throws Exception
   */
  Product getAllProductDetailsByCodeAndMarkForDeleteFalse(String code) throws Exception;

  /**
   * process vendor search auto heal products
   * @param storeId
   * @param productCode
   */
  void processVendorSearchAutoHealProduct(String storeId, String productCode);

  /**
   * Update brand in product,productAttributes and ProductItem Attributes
   *
   * @param changeBrandRequest
   * @return
   */
  void updateBrandInProductAndProductItems(ChangeBrandRequest changeBrandRequest) throws Exception;

  /**
   * to process products permanent delete event
   *
   * @param productCode
   * @param sellerCode
   */
  void processProductsPermanentDelete(String productCode, String sellerCode);

  /**
   *
   * @param categoryCode
   * @return CategoryDTO
   */
  CategoryDTO fetchParentCategoryFromCnCategoryCode(String categoryCode);

  /**
   *
   * @param productChange
   */
  void updateDistributionMappingStatusOnChange(ProductChange productChange);
}
