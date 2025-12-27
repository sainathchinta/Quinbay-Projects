package com.gdn.mta.product.service;

import java.io.IOException;
import java.util.List;
import java.util.Set;

import com.gda.mta.product.dto.AuditTrailDto;
import com.gda.mta.product.dto.AutoApprovalTypeRequest;
import com.gda.mta.product.dto.BulkMasterProductUpdateResponse;
import com.gda.mta.product.dto.EditProductResponse;
import com.gda.mta.product.dto.EditedResizeAndImagesUpdateStatusResponse;
import com.gda.mta.product.dto.NeedRevisionSubmitRequest;
import com.gda.mta.product.dto.ProductBrandUpdateRequest;
import com.gda.mta.product.dto.ProductRevisionInfoResponse;
import com.gda.mta.product.dto.RestrictedKeywordsByFieldAndActionType;
import com.gda.mta.product.dto.RetryNeedRevisionRequest;
import com.gda.mta.product.dto.ScreeningProductBulkActionsRequest;
import com.gda.mta.product.dto.VendorNotesRequest;
import com.gda.mta.product.dto.generator.StuckProductResponse;
import com.gda.mta.product.dto.response.AutoApprovalTypeResponse;
import com.gda.mta.product.dto.response.RetryAutoNeedRevisionResponse;
import com.gda.mta.product.dto.response.VendorNotesResponse;
import com.gdn.micro.graphics.domain.event.model.BulkImageProcessResponse;
import com.gdn.mta.domain.event.modal.AutoNeedRevisionDomainEvent;
import com.gdn.mta.domain.event.modal.ImageQcRequestDomainEvent;
import com.gdn.mta.domain.event.modal.ImageQcResponseDomainEvent;
import com.gdn.mta.domain.event.modal.VendorPublishEventModel;
import com.gdn.mta.product.entity.ProductCollection;
import com.gdn.mta.product.entity.UpdatedProductHistory;
import com.gdn.mta.product.enums.BulkActionType;
import com.gdn.mta.product.valueobject.BulkMasterProductUpdateRequestDTO;
import com.gdn.x.businesspartner.dto.ProfileResponse;
import com.gdn.x.productcategorybase.dto.CategorySummaryResponse;
import com.gdn.x.productcategorybase.entity.Product;

public interface ProductServiceWrapper {

  /**
   * bulk update for activated master products
   * @param bulkMasterProductUpdateRequestDTO
   * @param StoreId
   * @return
   */
  BulkMasterProductUpdateResponse bulkUpdateActivatedProducts(
      BulkMasterProductUpdateRequestDTO bulkMasterProductUpdateRequestDTO, String StoreId);

  /**
   *
   * @param storeId
   * @param bulkActionType
   * @param screeningProductBulkActionsRequest
   * @throws Exception
   */
  void doScreeningProductsBulkActions(String storeId, BulkActionType bulkActionType,
      ScreeningProductBulkActionsRequest screeningProductBulkActionsRequest) throws Exception;


  /**
   * update product
   * @param storeId
   * @param product
   * @param notes
   * @param marginExceed
   * @param brandCode
   * @param postLive
   * @param forceReviewNotes
   * @param brandApprovalStatus
   * @param onlyVatChanged
   */
  void update(String storeId, Product product, String notes, boolean marginExceed, String brandCode, boolean postLive,
    String forceReviewNotes, String brandApprovalStatus, boolean onlyVatChanged) throws Exception;

  /**
   * delete product after complete merge
   *
   * @param storeId
   * @param productId
   * @param productCode
   */
  void delete(String storeId, String productId, String productCode) throws Exception;

  /**
   * update product assignment status
   *
   * @param storeId
   * @param productCode
   * @param assignedTo
   * @param assignedBy
   */
  void updateProductAssignmentStatus(String storeId, String productCode, String assignedTo, String assignedBy);

  /**
   * Get revision history for the product
   * @param storeId
   * @param productCode
   * @return
   */
  List<ProductRevisionInfoResponse> getProductRevisionInfo(String storeId, String productCode);

  /**
   * Get screener notes
   *
   * @param storeId
   * @param productCode
   * @return
   */
  String getScreeningNotes(String storeId, String productCode);

  /**
   * change brand approval status and brand code in productCollection on approval and rejection of brand
   * @param brandCode
   * @param brandRequestCode
   * @param brandName
   */
  void changeBrandCodeAndBrandApprovalStatusInScreeningProducts(String brandCode, String brandRequestCode,
      String status, String brandName);

  /**
   * Resize images by product code or delta index
   *
   * @param storeId
   * @param productCode
   * @param deltaIndex
   * @param page
   * @param size
   */
  void resizeImages(String storeId, String productCode, boolean deltaIndex, int page, int size) throws Exception;


  /**
   * Update image path and skip screening for post live products
   *
   * @param bulkImageProcessResponse
   */
  void updateImagePathsAndSkipScreeningForPostLiveProducts(BulkImageProcessResponse bulkImageProcessResponse)
      throws Exception;


  /**
   * Update product post live status and skip screening
   *
   * @param productCollection
   */
  void updatePostLiveFlagAndSkipScreening(ProductCollection productCollection) throws Exception;

  /**
   * Update Image paths on resizing failure
   *
   * @param bulkImageProcessResponse
   */
  void updateImagePathsAndFlagOnResizingImageFailure(BulkImageProcessResponse bulkImageProcessResponse)
      throws Exception;

  /**
   * Update product category
   *
   * @param storeId
   * @param productCode
   * @param categoryCode
   * @param updateSalesCategory
   * @return
   */
  ProductCollection updateProductCategory(String storeId, String productCode, String categoryCode, boolean updateSalesCategory) throws Exception;

  /**
   * Check if product is eligible for screening skip
   *
   * @param productCollection
   * @return
   */
  boolean checkIfProductIsEligibleForScreeningSkip(ProductCollection productCollection) throws Exception;

    /**
     * Update product category in PCB
     *
     * @param productCode product code
     * @param categoryCode category code
     * @param updateSalesCategory update sales category flag
     * @param businessPartnerCode business partner code
     * @return CategorySummaryResponse
     */
  CategorySummaryResponse updateCategoryInPcb(String productCode, String categoryCode,
    boolean updateSalesCategory, String businessPartnerCode) throws Exception;

  /**
   * Process the image qc response
   *
   * @param storeId
   * @param imageQcResponseDomainEvent
   */
  void processImageQcResponse(String storeId, ImageQcResponseDomainEvent imageQcResponseDomainEvent) throws Exception;

  /**
   * update product l4 history on change of wholesale changes by scheduler
   *
   * @param merchantCode
   * @param itemSku
   * @param wholesalePriceActivated
   */
  void updateProductHistoryOnWholesaleChangesByScheduler(String merchantCode, String itemSku,
      boolean wholesalePriceActivated) throws Exception;

  /**
   * update product sku history
   *
   * @param auditTrailRequests
   * @param accessChannel
   * @param updateDirectly
   * @param historySolrUpdateNewEvent
   */
  void updateProductHistoryLevel3Audit(List<AuditTrailDto> auditTrailRequests, String accessChannel,
  boolean updateDirectly, boolean historySolrUpdateNewEvent) throws Exception;


  /**
   * update category change in PCB, PBP and History
   *
   * @param restrictedKeywordsByFieldAndActionType
   * @param productCollection
   * @param storeId
   * @param destinationCategoryCode
   * @param updateSalesCategory
   * @param profileResponse
   */
  ProductCollection categoryChangeInPbpPcbAndHistoryInCreationFlow(
      RestrictedKeywordsByFieldAndActionType restrictedKeywordsByFieldAndActionType,
      ProductCollection productCollection, String storeId, String destinationCategoryCode, boolean updateSalesCategory,
      ProfileResponse profileResponse) throws Exception;
  /**
   * performing action based on restricted keyword
   *
   * @param storeId
   * @param productCode
   * @param action
   * @param categoryRestrictedKeywordId
   * @param screeningAction
   * @param validateDraftState
   * @param currentlyAutoNeedRevisionState
   * @param vendorErrorFields
   * @param overrideDataFromPDT
   * @param businessPartnerCode
   * @param profileResponse
   * @param itemCodes
   * @param editResponse
   * @throws Exception
   */
  void performResultantActionBasedOnRestrictedKeywords(String storeId, String productCode, int action,
      String categoryRestrictedKeywordId, boolean screeningAction, boolean validateDraftState,
      boolean currentlyAutoNeedRevisionState, List<String> vendorErrorFields, boolean overrideDataFromPDT,
    String businessPartnerCode, ProfileResponse profileResponse, Set<String> itemCodes,
      EditProductResponse editResponse) throws Exception;

  /**
   * API to skip review and call the process image api for skip review true products
   *
   * @param productCode
   * @param profileResponse
   */
  void skipScreeningForSkipReviewProduct(String productCode, ProfileResponse profileResponse) throws Exception;

  /**
   * Publish solr update events
   *
   * @param audit
   */
  void publishSolrHistoryUpdateEvent(List<UpdatedProductHistory> audit);

  /**
   * Update image path for edited resized images
   *
   * @param bulkImageProcessResponse
   * @param isRevised
   */
  void updateImagePathsForEditedResizeImages(BulkImageProcessResponse bulkImageProcessResponse, boolean isRevised)
      throws Exception;


  /**
   * check and publish add edited event to pdt
   * @param vendorPublishEventModel vendorPublishEventModel
   */
  void processPcbVendorPublishEvent(VendorPublishEventModel vendorPublishEventModel) throws Exception;
  /**
   * Get vendor notes
   *
   * @param storeId
   * @param productCode
   * @return
   */
  VendorNotesResponse getVendorNotes(String storeId, String productCode) throws IOException;

  /**
   * update vendor notes
   *
   * @param storeId
   * @param productCode
   * @param vendorNotesRequest
   * @return
   */
  void updateVendorNotes(String storeId, String productCode, VendorNotesRequest vendorNotesRequest)
      throws IOException;

  /**
   * Need revision submit API
   *
   * @param storeId
   * @param username
   * @param needRevisionSubmitRequest
   * @return
   */
  EditProductResponse needRevisionSubmit(String storeId, String username,
      NeedRevisionSubmitRequest needRevisionSubmitRequest)
      throws Exception;

  /**
   * Auto approve product
   *
   * @param storeId
   * @param productCode
   */
  void autoApproveProduct(String storeId, String productCode) throws Exception;

  /**
   * Retry skip review product activation
   *
   * @param productCode
   */
  void retrySkipReviewProductActivation(String productCode) throws Exception;

  /**
   * do auto need revision of products
   * @param autoNeedRevisionDomainEvent
   * @param contentNeedRevision
   * @param screeningAction
   * @param validateDraftState
   * @param overrideDataFromPDT
   * @param validateAssignment
   */
  void autoNeedRevisionProduct(AutoNeedRevisionDomainEvent autoNeedRevisionDomainEvent, boolean contentNeedRevision,
      boolean screeningAction, boolean validateDraftState, boolean overrideDataFromPDT, boolean validateAssignment)
      throws Exception;

  /**
   * Retry auto need revision
   *
   * @param retryNeedRevisionRequest
   * @param validateAssignment
   * @return
   */
  RetryAutoNeedRevisionResponse retryAutoNeedRevision(RetryNeedRevisionRequest retryNeedRevisionRequest,
      boolean validateAssignment)
      throws Exception;

  /**
   * Publish revised event
   *
   * @param storeId
   * @param productCode
   */
  void publishRevisedEvent(String storeId, String productCode) throws Exception;

  /**
   * update review pending
   * @param storeId
   * @param productCode
   * @param reviewPending
   */
  void updateReviewPending(String storeId, String productCode, boolean reviewPending);

  /**
   * Update viewable flag
   *
   * @param storeId
   * @param productCode
   * @param activated
   * @param viewable
   */
  void updateActivatedAndViewable(String storeId, String productCode, boolean activated, boolean viewable);

  /**
   * Process Image qc for backlog products
   *  @param storeId
   * @param imageQcResponseDomainEvent
   */
  void processImageQcForBacklogProducts(String storeId, ImageQcResponseDomainEvent imageQcResponseDomainEvent)
      throws Exception;

  /**
   * Get image qc domain event
   *
   * @param productCode
   * @return
   * @throws Exception
   */
  ImageQcRequestDomainEvent getImageQcRequestDomainEvent(String productCode) throws Exception;

  /**
   * delete terminated seller products if its not a shared product
   *
   * @param storeId
   * @param productSku
   */
  void deleteTerminatedSellerNonSharedProducts(String storeId, String productSku);

  /**
   * retry stuck products
   * @param retryBatchSizeCount
   * @return
   */
  void getStuckProductCodeAndState(int retryBatchSizeCount)
      throws Exception;

  /**
   * Fetch auto approval enum by autoApprovalTypeRequest
   *
   * @param storeId
   * @param username
   * @param productCode
   * @param onlyCategoryChange
   * @param autoApprovalTypeRequest
   * @return
   */
  AutoApprovalTypeResponse findAutoApprovalTypeByRequest(String storeId, String username, String productCode,
      boolean onlyCategoryChange, AutoApprovalTypeRequest autoApprovalTypeRequest) throws Exception;

  /**
   * Publish edited image resize event
   *
   * @param productCode
   * @param editedResizeAndImagesUpdateStatusResponse
   */
  void publishEditedImageResizeEvent(String productCode,
      EditedResizeAndImagesUpdateStatusResponse editedResizeAndImagesUpdateStatusResponse);

  /**
   *
   * @param storeId
   * @param productCode
   * @throws Exception
   */
  void processProductVendorSearchAutoHeal(String storeId, String productCode) throws Exception;

  /**
   * delete terminated seller product from DB and Solr
   * @param productCode
   * @param sellerCode
   * @return
   */
  void terminatedSellerSkuCleanup(String productCode, String sellerCode) throws Exception;

  /**
   * Update the brand value of a product
   *
   * @param storeId
   * @param productBrandUpdateRequest
   * @throws Exception
   */
  void updateProductBrandValue(String storeId, ProductBrandUpdateRequest productBrandUpdateRequest)
      throws Exception;
}
