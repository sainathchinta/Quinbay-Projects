package com.gdn.partners.pcu.internal.service;

import java.util.List;

import com.gda.mta.product.dto.ProductAndL5MigrationRequest;
import com.gdn.partners.pcu.internal.client.model.request.ProductAttributeExtractionsRequest;
import com.gdn.partners.pcu.internal.web.model.request.AutoApprovalEligibilityWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.BigQueryFetchWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.DeleteProductWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.ItemViewConfigWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.PDTStateUpdateWebRequest;
import com.gdn.x.productcategorybase.dto.request.SystemParameterRequest;

public interface UtilityService {

  /**
   * Publish add edited event to PDT
   *
   * @param productCode
   * @param reviewType
   */
  void publishAddEditedEventToPDT(String productCode, String reviewType);

  /**
   * Publish add revised event to PDT
   *
   * @param productCode
   */
  void publishAddRevisedEventToPDT(String productCode);

  /**
   * Retry product image resize
   *
   * @param productCode
   */
  void retryProductImageResize(String productCode);

  /**
   * Retry edited product image resize
   *
   * @param productCode
   */
  void retryEditedProductImageResize(String productCode);

  /**
   * Update pbp product workflow
   *
   * @param productCode
   * @param state
   */
  void updatePBPProductWorkflow(String productCode, String state);

  /**
   * Update PBP review pending
   *
   * @param productCode
   * @param reviewPending
   */
  void updatePBPReviewPending(String productCode, boolean reviewPending);

  /**
   * Update activated and viewable
   *
   * @param productCode
   * @param activated
   * @param viewable
   */
  void updatePBPActivatedAndViewable(String productCode, boolean activated, boolean viewable);

  /**
   * Check auto approval eligibility
   *
   *
   * @param productCode
   * @param autoApprovalEligibilityWebRequest
   * @return
   */
  String checkProductAutoApprovalEligibility(String productCode, AutoApprovalEligibilityWebRequest autoApprovalEligibilityWebRequest);

  /**
   * Publish PCB event
   *  @param productCode
   * @param operationType
   */
  void republishPCBProductPublishEvent(String productCode, String operationType);

  /**
   * Clear PCB product cache
   *
   * @param productCode
   * @param productId
   */
  void clearPCBProductCache(String productCode, String productId);

  /**
   * Update PCB product viewable
   *
   * @param productCode
   * @param viewable
   */
  void updatePCBProductViewable(String productCode, boolean viewable);

  /**
   * Update PCB product viewable
   *
   * @param productCode
   * @param reviewPending
   */
  void updatePCBProductReviewPending(String productCode, boolean reviewPending);

  /**
   * Update PDT state
   *
   * @param productCode
   * @param pdtStateUpdateWebRequest
   */
  void updatePDTState(String productCode, PDTStateUpdateWebRequest pdtStateUpdateWebRequest);

  /**
   * Take down or re-activate in x-product
   *  @param productSku
   * @param forceReview
   * @param itemViewConfigWebRequests
   */
  void takeDownOrReactivateProduct(String productSku, boolean forceReview,
      List<ItemViewConfigWebRequest> itemViewConfigWebRequests);

  /**
   *
   * @param id bulk data table id to abort
   */
  void abortPendingBulkProcessById(String id);

  /**
   *
   * @param entity entity to abort
   * @param status status of the entity
   */
  void abortPendingDownloadsByEntity(String entity, String status);

  /**
   * @param userName                      user
   * @param requestId
   * @param storeId
   * @param clientId
   * @param channelId
   * @param needEmailNotification
   * @param deleteProductWebRequest       delete Request
   */
  void deleteProductCollection(String userName, String requestId,
    String storeId, String clientId, String channelId, boolean needEmailNotification,
    DeleteProductWebRequest deleteProductWebRequest);

  /**
   * Reindex active product by product code and store id
   *
   * @param productCode                      Not empty
   */
  void reindexActiveProductByProductCode(String productCode);

  /**
   * Fetch data from big Query for master sku review
   *
   * @param processName
   * @param request
   */
  void fetchDataFromBigQueryForMasterSkuReview(String processName, BigQueryFetchWebRequest request);

  /**
   * Fetch data from big Query for master sku review
   *
   * @param storeId string non null
   * @param productAndL5MigrationRequest migration request
   * @param username non null
   * @param channelId string
   * @param clientId string
   * @param requestId String
   */
  void migrateProductAndL5DetailsByProductSku(String storeId, String requestId, String clientId,
    String channelId, ProductAndL5MigrationRequest productAndL5MigrationRequest, String username);

  /**
   * Update system parameter in PCB
   *
   * @param storeId string non null
   * @param systemParameterRequest system Param Update request
   * @param channelId string
   * @param clientId string
   * @param requestId String
   */
  void updateSystemParameterInPCB(String storeId, String requestId, String channelId,
    String clientId, SystemParameterRequest systemParameterRequest);

  /** Generate product score api in xproduct
   * @param storeId
   * @param channelId
   * @param clientId
   * @param requestId
   * @param username
   * @param updateCategory
   * @param productSku
   * @param productCode
   */
  void generateProductScoreByProductSkuOrProductCode(String storeId, String channelId,
    String clientId, String requestId, String username, boolean updateCategory, String productSku,
    String productCode);

  /**
   * Publish product attribute extractions request to PCB
   * @param storeId
   * @param productAttributeExtractionsRequest
   */
  void publishProductAttributeExtractions(String storeId,
      ProductAttributeExtractionsRequest productAttributeExtractionsRequest);
}
