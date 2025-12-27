package com.gdn.mta.bulk.service;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.gdn.mta.bulk.dto.BulkReviewUploadModel;
import com.gdn.mta.bulk.dto.BulkBrandAuthUploadModel;
import com.gdn.mta.bulk.dto.BulkInternalProcessSummaryRequest;
import com.gdn.mta.bulk.dto.BulkInternalProcessSummaryResponse;
import com.gdn.mta.bulk.dto.BulkInternalProcessUploadRequest;
import com.gdn.mta.bulk.dto.BulkRestrictedKeywordUploadModel;
import com.gdn.mta.bulk.dto.BulkUpdateProcessDTO;
import com.gdn.mta.bulk.dto.BulkVendorProductAssignRequest;
import com.gdn.mta.bulk.dto.MasterDataBulkUpdateRequest;
import com.gdn.mta.bulk.models.InternalBrandUpdateEventModel;
import com.gdn.mta.bulk.models.InternalBulkUploadDataDomainEventModel;
import org.springframework.data.domain.Page;

import com.gdn.mta.bulk.models.InternalProcessDataDomainEventModel;

public interface InternalProcessServiceWrapper {
  /**
   * Fetching bulk internal process summary
   *
   * @param storeId
   * @param bulkInternalProcessSummaryRequest
   * @param page
   * @param size
   * @return
   */
  Page<BulkInternalProcessSummaryResponse> bulkInternalProcessSummary(String storeId,
      BulkInternalProcessSummaryRequest bulkInternalProcessSummaryRequest, int page, int size);

  /**
   * @param storeId
   * @param userName
   * @param processType
   */
  void processNewInternalProcessRequest(String storeId, String userName, String processType);

  /**
   * @param storeId
   * @param requestId
   * @param username
   * @param processType
   */
  void processInternalProcessDataRequest(String storeId, String requestId, String username, String processType);

  /**
   * @param storeId
   * @param processType
   * @param parentCode
   * @param internalProcessRequestId
   */
  void processUpdateSalesCategoryEvent(String storeId, String processType, String parentCode,
      String internalProcessRequestId);

  /**
   * process delete brand authorisation event
   * @param storeId
   * @param processType
   * @param internalProcessDataRequestId
   */
  void processDeleteBrandAuthorisationEvent(String storeId, String processType, String internalProcessDataRequestId);

  /**
   *
   * @param storeId
   * @param updatedBy
   * @param processType
   * @param internalProcessDataRequestId
   */
  void processInternalBulkUploadEvent(String storeId, String updatedBy, String processType,
      String internalProcessDataRequestId);

  /**
   * Api to process store copy product event
   *
   * @param storeCopyProductCreationDetails
   */
  void processEvent(InternalProcessDataDomainEventModel storeCopyProductCreationDetails) throws Exception;

  /**
   * @param storeId
   * @param processType
   */
  void deleteOldBulkInternalProcessRequest(String storeId, String processType);

  /**
   * @param storeId
   * @param processType
   */
  void abortPendingBulkInternalProcessBefore(String storeId, String processType);

  /**
   * @param storeId
   * @param processType
   */
  void failPendingBulkInternalProcessDataBefore(String storeId, String processType);

  /**
   * @param storeId
   * @param requestId
   * @param username
   * @param processType
   */
  void processStatusUpdate(String storeId, String requestId, String username, String processType);

  /**
   * Upload bulk internal process
   *
   * @param storeId
   * @param bulkInternalProcessUploadRequest
   */
  void uploadBulkInternalProcess(String storeId, BulkInternalProcessUploadRequest bulkInternalProcessUploadRequest);

  /**
   * Bulk internal process cancel request
   *
   * @param username
   * @param internalProcessRequestCode
   */
  void bulkInternalProcessCancelRequest(String storeId, String username, String internalProcessRequestCode);

  /**
   *
   * @param storeId
   * @param masterDataBulkUpdateRequest
   */
  void uploadInternalBulkUploadToBulkInternalProcess(String storeId, MasterDataBulkUpdateRequest masterDataBulkUpdateRequest);

  /**
   * Vendor Bulk asignment
   *
   * @param storeId
   * @param bulkVendorProductAssignRequest
   */
  void uploadVendorBulkAssignmentProcess(String storeId, BulkVendorProductAssignRequest bulkVendorProductAssignRequest);

  /**
   * Bulk Restricted Keyword Assignment
   *
   * @param storeId
   * @param bulkRestrictedKeywordUploadModel
   */
  void uploadBulkRestrictedKeywordProcess(String storeId,
      BulkRestrictedKeywordUploadModel bulkRestrictedKeywordUploadModel);

  /**
   * Bulk Brand Authorization
   *
   * @param storeId
   * @param bulkBrandAuthUploadModel
   */
  void uploadBulkBrandAuthProcess(String storeId, BulkBrandAuthUploadModel bulkBrandAuthUploadModel);

  /**
   * process bulk brand authorisation event
   * @param storeId
   * @param processType
   * @param internalProcessDataRequestId
   */
  void processBulkBrandAuthorisationEvent(String storeId, String processType, String internalProcessDataRequestId);

  /**
   * process bulk review event
   * @param storeId
   * @param bulkReviewUploadModel
   */
  void uploadBulkReviewProcess(String storeId, BulkReviewUploadModel bulkReviewUploadModel);

  /**
   * Process bulk approval or rejection
   * @param storeId 10001
   * @param processType BULK_REJECTION || BULK_APPROVAL
   * @param internalProcessDataRequestId unique id
   */
  void processBulkVendorActionsEvent(String storeId, String processType, String internalProcessDataRequestId);

  /**
   * to perform cluster action
   *
   * @param storeId                      storeId
   * @param internalProcessDataRequestId internalProcessDataRequestId
   * @throws Exception Exception
   */
  void processBulkMasterSkuReviewDataEvent(String storeId, String internalProcessDataRequestId) throws Exception;

  /**
   * to process bulk assignee for master-sku-review
   *
   * @param storeId
   * @param internalProcessDataRequestId
   */
  void processBulkMasterSkuAssigneeEvent(String storeId, String internalProcessDataRequestId);

  /**
   * to process bulk assign for auto approved products
   *
   * @param storeId
   * @param internalProcessDataRequestId
   */
  void processAutoApprovedProductsBulkAssignEvent(String storeId, String internalProcessDataRequestId);

  /**
   * Pre Process Bulk Price Rebate
   * @param bulkUpdateProcessDTO
   */
  void preProcessBulkPriceRecommendationFile(BulkUpdateProcessDTO bulkUpdateProcessDTO) throws Exception;
  /**
   * Process internal bulk price update
   *
   * @param internalProcessDataDomainEventModel
   * @param updateCampaignPrice
   */
  void processBulkInternalBulkPriceUpdate(InternalBulkUploadDataDomainEventModel internalProcessDataDomainEventModel,
      boolean updateCampaignPrice);

  /**
   * Process Bulk Price Rebate Upload
   * @param storeId
   * @param internalProcessDataRequestId
   */
  void processBulkRebateUpload(String storeId, String internalProcessDataRequestId);
  /**
   * Process Bulk Product Type Tagging Update
   *
   * @param internalBulkUploadData
   */
  void processBulkProductTypeTaggingUpdate(InternalBulkUploadDataDomainEventModel internalBulkUploadData)
    throws JsonProcessingException;

  /**
   * Process Bulk Add or Review for IPR Products
   *
   * @param storeId
   * @param internalProcessDataRequestId
   */
  void processIPRProductsBulkAddReviewEvent(String storeId, String username, String internalProcessDataRequestId);

  /**
   * Process sku level rebate update
   *
   * @param internalProcessDataDomainEventModel
   */
  void processBulkSkuLevelRebateUpload(InternalBulkUploadDataDomainEventModel internalProcessDataDomainEventModel);

  /**
   * process internal brand update event
   *
   * @param internalBrandUpdateEventModel
   */
  void processInternalBrandUpdateEvent(InternalBrandUpdateEventModel internalBrandUpdateEventModel);
}
