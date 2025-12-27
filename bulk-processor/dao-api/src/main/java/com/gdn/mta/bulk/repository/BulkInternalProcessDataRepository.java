package com.gdn.mta.bulk.repository;

import java.util.Date;
import java.util.List;
import java.util.Set;

import com.gdn.mta.bulk.ProcessStatus;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import com.gdn.mta.bulk.dto.BulkInternalProcessPendingDataDTO;
import com.gdn.mta.bulk.entity.BulkInternalProcessData;

public interface BulkInternalProcessDataRepository extends JpaRepository<BulkInternalProcessData, String> {

  List<BulkInternalProcessData> findByParentCodeAndProcessTypeAndInternalProcessRequestIdAndAndMarkForDeleteFalse(
      String parentCode, String processType, String internalProcessRequestId);

  List<BulkInternalProcessData> findByParentCodeAndProcessTypeAndStatusAndInternalProcessRequestIdAndAndMarkForDeleteFalse(
    String parentCode, String processType, String processStatus, String internalProcessRequestId);

  List<BulkInternalProcessData> findByParentCodeAndProcessTypeAndInternalProcessRequestIdAndStatusAndMarkForDeleteFalse(
      String parentCode, String processType, String internalProcessRequestId, String status);

  @Query(value = "SELECT internal_process_request_id, parent_code, process_type, internal_process_request_code "
      + "FROM blp_internal_process_data "
      + "WHERE store_id = ?1 AND status= ?2 AND process_type= ?3 AND mark_for_delete = FALSE AND internal_process_request_id IN (?4) "
      + "GROUP BY internal_process_request_id, parent_code, process_type, internal_process_request_code "
      + "ORDER BY internal_process_request_code "
      + "LIMIT ?5", nativeQuery = true)
  List<Object[]> getDistinctParentCodeByStatusAndProcessType(String storeId, String status,
      String processType, List<String> internalProcessRequestIds, Integer fetchBatchSize);

  @Query("SELECT COUNT(1) FROM BulkInternalProcessData blpd WHERE blpd.storeId = :storeId AND blpd.status = :status AND "
      + "blpd.internalProcessRequestId = :internalProcessRequestId")
  int getCountByStoreIdAndStatusAndInternalProcessRequestId(@Param("storeId") String storeId,
      @Param("status") String status, @Param("internalProcessRequestId") String internalProcessRequestId);


  void deleteByInternalProcessRequestId(String internalProcessRequestId);

  List<BulkInternalProcessData> findByStoreIdAndInternalProcessRequestId(String storeId,
      String internalProcessRequestId);

  BulkInternalProcessData findByStoreIdAndId(String storeId, String id);

  List<BulkInternalProcessData> findByStoreIdAndInternalProcessRequestCodeAndStatus(String storeId,
      String internalProcessRequestCode, String status);

  @Modifying
  @Query("UPDATE BulkInternalProcessData SET status='FAILED', updatedBy = 'PENDING_TO_FAILED_RUNDECK',"
      + "updatedDate = CURRENT_TIMESTAMP, errorMessage = 'Fail unprocessed published request data' WHERE status IN "
      + "('IN_PROGRESS', 'PUBLISHED', 'PROCESSING', 'PICKED') AND processType = ?2 AND updatedDate < ?1")
  void updateStatusInProgressBulkInternalProcessToFailed(Date updatedDate, String processType);

  @Query(value = "SELECT * FROM blp_internal_process_data WHERE store_id = ?1 AND mark_for_delete = false AND internal_process_request_id IN (?2) ORDER BY created_date LIMIT ?3", nativeQuery = true)
  List<BulkInternalProcessData> findByStoreIdAndMarkForDeleteFalseAndInternalProcessRequestIdIn(String storeId,
      List<String> internalProcessRequestIds, int limit);

  List<BulkInternalProcessData> findByStoreIdAndInternalProcessRequestIdInAndStatusAndMarkForDeleteFalse(String storeId,
      List<String> internalProcessRequestIds, String status, Pageable pageRequest);

  List<BulkInternalProcessData> findByStoreIdAndInternalProcessRequestCodeAndParentCodeIn(String storeId,
      String internalProcessRequestCode, List<String> parentCode) throws Exception;

  BulkInternalProcessData findByStoreIdAndIdAndStatus(String storeId, String id, String status);

  List<BulkInternalProcessData> findByStoreIdAndIdInAndStatus(String storeId, List<String> id, String status);

  List<BulkInternalProcessData> findByStoreIdAndInternalProcessRequestIdAndStatus(String storeId,
    String internalProcessRequestCode, String status);

  @Query(value = "SELECT DISTINCT(parent_code) FROM blp_internal_process_data WHERE store_id = ?1 AND internal_process_request_id = ?2 AND STATUS = 'PENDING'", nativeQuery = true)
  Set<String> findParentCodeByStoreIdAndInternalProcessRequestIdAndStatus(String storeId, String internalProcessRequestId);

  @Modifying
  @Query(value = "UPDATE blp_internal_process_data SET status = ?1, updated_by = ?2, updated_date = CURRENT_TIMESTAMP WHERE store_id = ?3 AND internal_process_request_id = ?4 AND parent_code IN ?5", nativeQuery = true)
  void updateStatusByParentCodeAndStoreIdAndInternalProcessRequestIdAndStatus(String status, String updatedBy,
      String storeId, String internalProcessRequestId, Set<String> parentCode);

}
