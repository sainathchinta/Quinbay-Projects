package com.gdn.mta.bulk.service;

import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.Set;

import com.gdn.mta.bulk.models.download.responsedata.BulkPriceProductTypeTaggingRequest;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;

import com.gdn.mta.bulk.dto.BulkInternalProcessPendingDataDTO;
import com.gdn.mta.bulk.dto.BulkInternalProcessSummaryRequest;
import com.gdn.mta.bulk.dto.InternalProcessPendingFilesResponse;
import com.gdn.mta.bulk.entity.BulkInternalProcess;
import com.gdn.mta.bulk.entity.BulkInternalProcessData;

public interface InternalProcessService {

  /**
   * @param storeId
   * @param status
   * @param pageRequest
   * @param processType
   * @return
   */
  Page<BulkInternalProcess> getAllBulkInternalProcessByStatus(String storeId, String status, Pageable pageRequest,
      String processType);

  /**
   * Fetching bulk internal process summary
   *
   * @param storeId
   * @param bulkInternalProcessSummaryRequest
   * @param page
   * @param size
   * @return
   */
  Page<BulkInternalProcess> bulkInternalProcessSummary(String storeId,
      BulkInternalProcessSummaryRequest bulkInternalProcessSummaryRequest, int page, int size);

  /**
   * Bulk internal process cancel request
   *
   * @param username
   * @param internalProcessRequestCode
   */
  void bulkInternalProcessCancelRequest(String storeId, String username, String internalProcessRequestCode);

  /**
   * @param bulkInternalProcess
   */
  BulkInternalProcess saveInternalProcess(BulkInternalProcess bulkInternalProcess);

  /**
   * @param bulkInternalProcessData
   */
  List<BulkInternalProcessData> saveInternalProcessData(List<BulkInternalProcessData> bulkInternalProcessData);

  /**
   * @param name
   * @param fetchBatchSize
   * @param processType
   * @param storeId
   * @param bulkInternalProcessList
   * @return
   */
  Map<String, List<BulkInternalProcessPendingDataDTO>> getAllBulkInternalProcessDataByStatusAndBatchSize(String name,
      Integer fetchBatchSize, String processType, String storeId, List<BulkInternalProcess> bulkInternalProcessList);


  /**
   * @param parentCode
   * @param processType
   * @param internalProcessRequestId
   * @return
   */
  List<BulkInternalProcessData> getAllProductByParentCodeAndProcessTypeAndInternalProcessRequestId(String parentCode,
      String processType, String internalProcessRequestId);

  /**
   * @param parentCode
   * @param processType
   * @param internalProcessRequestId
   * @param status
   * @return
   */
  List<BulkInternalProcessData> findByParentCodeAndProcessTypeAndInternalProcessRequestIdAndStatusAndMarkForDeleteFalse(
      String parentCode, String processType, String internalProcessRequestId, String status);

  /**
   * @param bulkInternalProcessList
   * @return
   */
  List<BulkInternalProcess> saveInternalProcesses(List<BulkInternalProcess> bulkInternalProcessList);

  /**
   * @param storeId
   * @param pendingToAbortDate
   * @param processType
   */
  void abortPendingBulkInternalProcess(String storeId, Date pendingToAbortDate, String processType);

  /**
   * @param storeId
   * @param pendingToAbortDate
   * @param processType
   */
  void failPendingBulkInternalProcessData(String storeId, Date pendingToAbortDate, String processType);

  /**
   * @param status
   * @param internalProcessRequestId
   * @return
   */
  int getCountByStoreIdAndStatusAndInternalProcessRequestId(String storeId, String status,
      String internalProcessRequestId);

  /**
   * @param storeId
   * @param processType
   * @param date
   * @param status
   * @param pageable
   * @return
   */
  Page<BulkInternalProcess> getBulkInternalProcessByProcessTypeAndStatusAndCreatedDate(String storeId,
      String processType, Date date, List<String> status, Pageable pageable);

  /**
   * @param bulkInternalProcessList
   */
  void deleteBulkInternalProcess(List<BulkInternalProcess> bulkInternalProcessList);

  /**
   * @param internalProcessRequestId
   */
  void deleteBulkInternalProcessDataByInternalRequestId(String internalProcessRequestId);

  /**
   * @param storeId
   * @param id
   * @return
   */
  List<BulkInternalProcessData> getAllBulkInternalProcessDataByStoreIdAndInternalProcessRequestId(String storeId,
      String id);

  /**
   * @param storeId
   * @param internalProcessRequestCode
   * @param status
   * @return
   */
  List<BulkInternalProcessData> getBulkInternalProcessDataByStoreIdAndInternalProcessRequestCodeAndStatus(
      String storeId, String internalProcessRequestCode, String status);

  /**
   * @param storeId
   * @param internalRequestIds
   * @param size
   * @return
   */
  List<BulkInternalProcessData> getBulkInternalProcessDataByRequestIds(String storeId, List<String> internalRequestIds,
      int size);

  /**
   *
   * @param storeId
   * @param internalRequestIds
   * @param status
   * @param size
   * @return
   */
  List<BulkInternalProcessData> getBulkInternalProcessDataByRequestIdsAndStatus(String storeId,
      List<String> internalRequestIds, String status, int size);

  /**
   * @param bulkInternalProcessData
   * @return
   */
  BulkInternalProcessData saveBulkInternalProcessData(BulkInternalProcessData bulkInternalProcessData);

  /**
   *
   * @param storeId
   * @param id
   * @return
   */
  BulkInternalProcessData getBulkInternalProcessDataById(String storeId, String id);

  /**
   * @param storeId
   * @param internalProcessRequestCode
   * @return
   */
  BulkInternalProcess findByInternalProcessRequestCode(String storeId, String internalProcessRequestCode);

  /**
   * fetch details based on bulk process and rows numbers
   *
   * @param storeId
   * @param internalProcessRequestCode
   * @param parentCode
   * @return
   */
  List<BulkInternalProcessData> findByStoreIdAndBulkProcessCodeAndRowNumberIn(String storeId,
      String internalProcessRequestCode, List<String> parentCode) throws Exception;

  /**
   * to check pending files of a user for given process type
   *
   * @param storeId
   * @param username
   * @param processType
   * @return
   */
  InternalProcessPendingFilesResponse checkPendingFiles(String storeId, String username, String processType);

  /**
   * Get internal process products for category update
   *
   * @param parentCode
   * @param processType
   * @param processStatus
   * @param internalProcessRequestId
   * @return
   */
  List<BulkInternalProcessData> getAllProductByParentCodeAndProcessTypeAndStatusAndInternalProcessRequestId(
    String parentCode, String processType, String processStatus,
    String internalProcessRequestId);

  /**
   *
   * @param storeId
   * @param id
   * @param status
   * @return
   */
  BulkInternalProcessData bulkInternalProcessDataByIdAndStatus(String storeId, String id, String status);

  /**
   * Find internal bulk process by storeId and ids and status
   *
   * @param storeId
   * @param ids
   * @param status
   * @return
   */
  List<BulkInternalProcessData> bulkInternalProcessDataByIdInAndStatus(String storeId, List<String> ids, String status);

  /**
   *
   * @param storeId
   * @param sellerCode
   * @param notes
   * @return Pending or in-progress fbb l5 creation
   */
  BulkInternalProcess checkPendingFbbL5Process(String storeId, String sellerCode, String notes);

  /**
   * @param storeId
   * @param internalProcessRequestId
   * @param status
   * @return
   */
  List<BulkInternalProcessData> getBulkInternalProcessDataByStoreIdAndInternalProcessRequestIdAndStatus(
    String storeId, String internalProcessRequestId, String status);

  Page<BulkInternalProcess> getAllBulkInternalProcessByStatusOrderByDateAsc(String storeId,
    String status, Pageable pageRequest,
    String processType);

  long countByStoreIdAndProcessTypeAndStatusIn(String storeId, String processType,
    List<String> status);


  /**
   * Get internal process By Process type and status list
   *
   * @param storeId
   * @param processType
   * @param status
   * @return
   */
  BulkInternalProcess findFirstByStoreIdAndProcessTypeAndStatusIn(String storeId, String processType,
    List<String> status);

  /**
   *
   * @param storeId
   * @param internalBulkRequestId
   * @return
   */
  Set<String> getDistinctCategoryCodeForRestrictedKeywordBulkUpdate(String storeId, String internalBulkRequestId);

  /**
   *
   * @param status
   * @param updatedBy
   * @param storeId
   * @param internalProcessRequestId
   * @param parentCode
   */
  void bulkInterUpdatedStatusForRestrictedKeywordBulkUpdate(String status, String updatedBy, String storeId,
      String internalProcessRequestId, Set<String> parentCode);

  /**
   * update Remove Bulk ProductType Tagging
   *
   * @param bulkInternalProcessDataList list of bulkInternalProcessData
   * @param updatedBy
   * @return
   */
  Map<String, String> updateRemoveBulkProductTypeTagging(List<BulkPriceProductTypeTaggingRequest> bulkInternalProcessDataList,
    String updatedBy);

  /**
   * Count number of pending requests
   *
   * @param storeId
   * @param processType
   * @param stateList
   * @param userName
   * @return
   */
  long countByStoreIdAndProcessTypeAndStatusInAndCreatedByAndMarkForDeleteFalse(String storeId, String processType,
      List<String> stateList, String userName);
}
