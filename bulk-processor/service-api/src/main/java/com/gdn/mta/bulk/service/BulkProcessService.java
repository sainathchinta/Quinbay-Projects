package com.gdn.mta.bulk.service;

import java.util.List;
import java.util.Map;
import java.util.Optional;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.gdn.mta.bulk.dto.BrandAndCategoryPredictionRequest;
import com.gdn.mta.bulk.dto.BulkProcessStatusListingResponse;
import com.gdn.mta.bulk.dto.BulkUpdateProcessDTO;
import com.gdn.mta.bulk.dto.GenericTemplateFileType;
import com.gdn.mta.bulk.entity.BulkProcessDataEstimation;
import com.gdn.mta.bulk.dto.QRCodeExcelQueue;
import com.gdn.mta.bulk.dto.QrExcelUploadRequest;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;

import com.gdn.common.exception.ApplicationException;
import com.gdn.mta.bulk.dto.BulkPendingRequestsResponse;
import com.gdn.mta.bulk.dto.BulkProcessNotesResponse;
import com.gdn.mta.bulk.dto.WholeSaleCountResponse;
import com.gdn.mta.bulk.entity.BulkProcess;
import com.gdn.mta.bulk.entity.BulkUpdateQueue;

public interface BulkProcessService {

  void deleteByBulkProcessCode(String storeId, String bulkProcessCode) throws ApplicationException;

  BulkProcess findByBulkProcessCode(String storeId, String bulkProcessCode);

  BulkProcess findByStoreIdAndBulkProcessCodeAndStatus(String storeId, String bulkProcessCode, String status);

  Page<BulkProcess> findByBusinessPartnerCodeAndBulkProcessTypeAndCreatedBy(String storeId,
      String businessPartnerCode, String bulkProcessType, Pageable pageable);

  Page<BulkProcess> findByBusinessPartnerCodeAndCreatedBy(String storeId, String businessPartnerCode, String createdBy,
      Pageable pageable);

  void updateBulkProcessRecordAsMarkForDeleteTrue(String date);

  /**
   * save bulk process
   * @param bulkProcess
   * @return
   */
  BulkProcess saveOperation(BulkProcess bulkProcess);


  /**
   * save bulk process
   *
   * @param bulkProcess
   * @return
   */
  BulkProcess saveBulkProcess(BulkProcess bulkProcess);

  /**
   * save list of bulk process
   * @param bulkProcessList
   * @return
   */
  List<BulkProcess> saveBulkProcessList(List<BulkProcess> bulkProcessList);

  /**
   * check pending bulk upload or download process
   * @param storeId
   * @param username
   * @param type
   * @param businessPartnerCode
   * @param systemParameterConfigName
   * @param bulkProcessType
   * @return
   */
  BulkPendingRequestsResponse checkForPendingBulkProcess(String storeId, String username, String type,
      String businessPartnerCode, String systemParameterConfigName, String bulkProcessType);
  
  /**
   * Deletes files older than certain number of days
   *
   * @param storeId
   */
  void findAndDeleteBulkProcessCodesBeforeXDays(String storeId);

  /**
   * Abort all pending bulk processes updated before date
   * @param storeId
   */
  void abortPendingBulkProcessBefore(String storeId);

  /**
   * Abort Bulk Process
   * @param storeId
   * @param bulkProcessCode
   */
  BulkProcess abortBulkProcess(String storeId, String bulkProcessCode) throws Exception;

  BulkProcess setErrorCountAndTotalCountAndSave(BulkProcess bulkProcess, int errorCount,
      int totalCount);

  /**
   *
   * @param storeId
   * @param bulkProcessCode
   * @throws Exception
   */
  void abortPendingInprogressBulkProcess(String storeId, String bulkProcessCode) throws Exception;

  /**
   * Fetch promo related notes of bulk process by bulk process code
   *
   * @param storeId
   * @param bulkProcessCode
   * @return
   * @throws Exception
   */
  List<BulkProcessNotesResponse> filterPromoBulkProcessNotes(String storeId, String bulkProcessCode) throws Exception;

  /**
   *
   * @param storeId
   * @param bulkProcessCode
   * @return
   * @throws Exception
   */
  WholeSaleCountResponse filterWholeSaleConfigBulkProcessNotes(String storeId, String bulkProcessCode) throws Exception;

  /**
   * Regenerate category and attribute mapping values in generic template
   *
   * @return
   */
  void regenerateCategoryAttributeMappingInGenericBulkTemplate() throws Exception;

  /**
   * Regenerate brand values in generic template and save the modified excel sheet
   *
   * @return
   * @param fileType
   */
  void regenerateBrandValuesInGenericBulkTemplate(String fileType);

  /**
   * Regenerate Master brand values in generic template and save the modified excel sheet
   *
   * @return
   * @param fileType
   */
  void regenerateMasterBrandValuesInGenericBulkTemplate(String fileType);

  /**
   *
   */
  void regenerateBrandValuesInCategoryTemplate();

  /**
   *
   * @param storeId
   * @param username
   * @param emailTo
   * @param requestId
   * @param parentCategoryCode
   * @param language
   * @throws Exception
   */
  void downloadUnmappedProductSkus(String storeId, String username, String emailTo, String requestId,
      String parentCategoryCode, String language) throws Exception;

  /**
   *
   * @param storeId
   * @param bulkProcessType
   * @param createdBy
   * @param status
   * @return
   */
  long countNumberOfUploadsByUser(String storeId, String bulkProcessType, String createdBy, String status);

  /**
   * @param storeId
   * @throws Exception
   * */
  void sendMailIfPendingRequestMoreThanThreshold(String storeId) throws Exception;

  /**
   * Bulk update off2on flag
   *
   * @param stringBooleanMap
   * @param requestId
   * @param username
   * @return
   */
  List<String> bulkUpdateOff2On(Map<String, Boolean> stringBooleanMap, String requestId, String username);

  /**
   * @param storeId
   * @param requestId
   * @param userName
   * @param businessPartnerCode
   * @param bulkProcessCode
   * @param filePath
   */
  void preProcessSubjectToVatUploadEvent(String storeId, String requestId, String userName, String businessPartnerCode,
      String bulkProcessCode, String filePath) throws Exception;

  /**
   * Download images
   *
   * @param bulkProcessCode
   * @param imageList
   */
  void downloadImages(String bulkProcessCode, List<String> imageList) throws Exception;

  /**
   * Publish images for download
   *
   * @param bulkProcessCode
   * @param bulkProcessType
   * @param imageList
   * @param priorityQueueEnabled
   */
  void publishBulkImageDownloadEventModel(String bulkProcessCode, String bulkProcessType, List<String> imageList, boolean priorityQueueEnabled);

  /**
   * Publish images for download
   *
   * @param bulkProcessCode
   * @param bulkProcessType
   * @param imageList
   */
  void publishBulkExternalImageDownloadEventModel(String bulkProcessCode, String bulkProcessType,
    List<String> imageList);

  /**
   * Publish images for download for basic info
   *
   * @param bulkProcessCode String
   * @param bulkProcessType String
   * @param imageList       List<String>
   */
  void publishBulkBasicInfoImageDownloadEventModel(String bulkProcessCode, String bulkProcessType,
      List<String> imageList);

  /**
   * Publish images for download for basic info
   *
   * @param bulkProcessCode     String
   * @param bulkProcessType     String
   * @param videoUrl            String
   * @param businessPartnerCode
   */

  void publishBulkBasicInfoVideoDownloadEventModel(String bulkProcessCode, String bulkProcessType,
      String videoUrl, String businessPartnerCode);

  /**
   * Check bulk process status
   *
   * @param storeId
   * @param bulkProcessType
   */
  void checkBulkProcessStatus(String storeId, String bulkProcessType) throws Exception;

  /**
   * Api to publish BULK_GENERIC_CREATE_PRODUCT_EVENT
   *
   * @param storeId
   * @param bulkProcess
   * @param bulkProcessType
   */
  void publishBulkProductCreationEvent(String storeId, BulkProcess bulkProcess,
    String bulkProcessType) throws Exception;

  /**
   * @param storeId
   * @param bulkProcess
   * @throws Exception
   */
  void publishBulkUpdateEvent(String storeId, BulkProcess bulkProcess) throws Exception;

  /**
   * @param storeId
   * @param bulkProcess
   * @throws Exception
   */
  void publishBulkUpdateEANEvent(String storeId, BulkProcess bulkProcess) throws Exception;

  /**
   * @param storeId
   * @param bulkProcess
   * @throws Exception
   */
  void publishBulkDeleteItemPickupPointEvent(String storeId, BulkProcess bulkProcess) throws Exception;

  /**
   *
   * @param storeId
   * @param bulkProcess
   * @throws Exception
   */
  void publishCampaignUploadEvent(String storeId, BulkProcess bulkProcess) throws Exception;

  /**
   *
   * @param storeId
   */
  void deleteFromDb(String storeId);


  /**
   * Api to check the stuck process status
   */
  void checkStuckProcessStatus(String bulkProcessCode);

  /**
   * API to publish instant pickup point upsert event
   *
   * @param storeId
   * @param bulkProcess
   * @throws Exception
   */
  void publishBulkInstantPickupItemUpsertEvent(String storeId, BulkProcess bulkProcess) throws Exception;

  /**
   * API to publish instant pickup point delete event
   *
   * @param storeId
   * @param bulkProcess
   * @throws Exception
   */
  void publishBulkInstantPickupItemDeleteEvent(String storeId, BulkProcess bulkProcess) throws Exception;

  /**
   * API to publish instore event
   *
   * publish instore event for processing
   */
  void publishInstoreUpdateEvent(String storeId, BulkProcess bulkProcess);

  /**
   * Publish archive product event
   *
   * @param storeId
   * @param bulkProcess
   */
  void publishArchiveProductEvent(String storeId, BulkProcess bulkProcess);

  /**
   * Publish vat update event
   *
   * @param storeId
   * @param bulkProcess
   */
  void publishVatUpdateEvent(String storeId, BulkProcess bulkProcess);

  /**
   *
   * @param storeId
   * @param businessPartnerCode
   * @param pickupPointCode
   * @return
   */
  List<BulkProcess> findByStoreIdAndBusinessPartnerCodeAndBulkProcessTypeAndPPCode(String storeId,
      String businessPartnerCode, String bulkProcessType, String pickupPointCode);

  /**
   * find bulkProcessType by type and status
   *
   * @param storeId
   * @param bulkProcessType
   * @param status
   * @param pageable
   * @return
   */
  List<BulkProcess> findByBulkProcessTypeAndStatus(String storeId, String bulkProcessType, String status,
      Pageable pageable);


  /**
   * Bulk Process Listing response to fetch process status
   *
   * @param storeId
   * @param bulkProcessType     ProductCreationUploadPriority,ProductLevel3
   * @param requestId           requestID
   * @param businessPartnerCode merchant code
   * @param bulkProcessCodes    Optional Param to be sent when switch is False
   * @param primaryIdentifier   Optional Param to be used to signify the campaign code for
   *                            campaign upload and downloads
   * @param estimationsNeeded   boolean will be true if ETA calculation is needed
   * @param pageable            page and Size defaulted to 50x
   */
  Page<BulkProcessStatusListingResponse> fetchProcessListingResponse(String storeId,
      String bulkProcessType, String requestId, String businessPartnerCode,
      Optional<List<String>> bulkProcessCodes, Optional<String> primaryIdentifier,
      boolean estimationsNeeded, Pageable pageable)
      throws  Exception;

  boolean checkForQrProcessAllowed(String storeId, String businessPartnerCode);

  /**
   * publish QR code generation event
   * @param storeId
   * @param bulkProcess
   */
  void publishForQRCodeGenerationEvent(String storeId, BulkProcess bulkProcess);

  /**
   * Upload qr excel request
   * @param storeId must not be blank
   * @param qrExcelUploadRequest must not be null
   * @param requestId must not be blank
   * @param username
   */
  void insertQrExcelRequest(String storeId, QrExcelUploadRequest qrExcelUploadRequest,
    String requestId, String username) throws Exception;

  /**
   * Process QR Code upload event and it's excel
   *
   * @param qrCodeExcelQueue must not be null
   */
  void insertQrExcelRequest(QRCodeExcelQueue qrCodeExcelQueue) throws Exception;

  /**
   * Evaluates the Bulk Process Estimation
   * @param storeId 10001
   * @param username run deck
   **/
  void evaluateBulkProcessEstimation(String storeId, String username)
    throws JsonProcessingException;


  /**
   * Utility to set Estimated Time for Process completion at Both Record and Process Level
   *
   * @param bulkProcessStatusListingResponse listing response for bulk creation and uploads
   * @param bulkProcessDataEstimations       bulkProcess Estimations
   * @param bulkProcessCodeXEstimationsMap
   **/
  void setEstimationsForListingResponses(BulkProcessStatusListingResponse bulkProcessStatusListingResponse,
    List<BulkProcessDataEstimation> bulkProcessDataEstimations, Map<String, Double> bulkProcessCodeXEstimationsMap);

  /**
   *
   * @param storeId
   * @param bulkProcessCode
   */
  void deleteBulkProcessDataByBulkProcessCode(String storeId, String bulkProcessCode);

  /**
   * Pre-process bulk work order request
   * @param storeId
   * @param requestId
   * @param bulkWorkOrderDTO
   */
  void preProcessWorkOrder(String storeId, String requestId, BulkUpdateProcessDTO bulkWorkOrderDTO) throws Exception;

  /**
   * Validate bulk process state
   * @param bulkUpdateQueue
   */
  BulkProcess validateAndUpdateWorkOrder(BulkUpdateQueue bulkUpdateQueue);

  /**
   * Process Bulk Work Order
   * @param bulkUpdateQueue
   */
  void processWorkOrder(BulkUpdateQueue bulkUpdateQueue, BulkProcess bulkProcess);

  /**
   * Process Work order upload event
   *
   * @param bulkProcess must not be null
   * @param storeId must not be null
   */
  void publishBulkWorkOrderUploadEvent(String storeId, BulkProcess bulkProcess);

  /**
   * abort Struck Processes By ProcessTypes
   *
   * @param processTypeXAbortTimeMap must not be empty
   * @param storeId must not be null
   */
  void abortStruckProcessesByProcessTypes(String storeId, Map<String, Integer> processTypeXAbortTimeMap);

  /**
   * Regenerate category and attribute mapping values in generic template by file type
   *
   * @param storeId                 must not be null
   * @param genericTemplateFileType must not be null
   * @param requestId               must not be null
   */
  void regenerateTemplateByFileType(String storeId, GenericTemplateFileType genericTemplateFileType, String requestId);

  /**
   * Publish basic info update event
   *
   * @param storeId
   * @param bulkProcess
   */
  void publishBasicInfoUpdateEvent(String storeId, BulkProcess bulkProcess);

  /**
   * Publish External creation image event
   *
   * @param bulkProcess bulkProcess
   * @param productIdToImageQCModelMap productIdToImageQCModelMap
   */

  void publishBulkExternalImageQCDownloadEventModel(BulkProcess bulkProcess,
    Map<String, BrandAndCategoryPredictionRequest> productIdToImageQCModelMap);

  /**
   * Find bulk process by status
   *
   * @param storeId         storeId
   * @param bulkProcessCode bulkProcessCode
   * @param status          status
   */
  BulkProcess findByStoreIdAndBulkProcessCodeAndStatusAndMarkForDeleteFalse(String storeId,
    String bulkProcessCode, String status);
}
