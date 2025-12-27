package com.gdn.mta.bulk.repository;

import java.util.Date;
import java.util.List;
import java.util.Optional;

import com.gdn.mta.bulk.dto.BulkProcessDataDTO;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import com.gdn.mta.bulk.dto.RowNumberParentCodeDTO;
import com.gdn.mta.bulk.entity.BulkProcessData;

public interface BulkProcessDataRepository extends JpaRepository<BulkProcessData, String> {

  List<BulkProcessData> findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(String storeId, String bulkProcessCode);

  Optional<BulkProcessData> findById(String id);

  List<BulkProcessData> findByStoreIdAndBulkProcessCodeAndStatusAndMarkForDeleteFalse(String storeId,
      String bulkProcessCode, String status) throws Exception;

  List<BulkProcessData> findByStoreIdAndBulkProcessCodeAndStatusInAndMarkForDeleteFalse(String storeId,
      String bulkProcessCode, List<String> status) throws Exception;

  List<BulkProcessData> findByStoreIdAndBulkProcessCodeAndParentProductAndStatusAndMarkForDeleteFalse(String storeId,
      String bulkProcessCode, String parentProduct, String status) throws Exception;

  List<BulkProcessData> findByStoreIdAndBulkProcessIdAndParentProductAndStatusAndMarkForDeleteFalse(String storeId,
      String bulkProcessId, String parentProduct, String status) throws Exception;

  @Query(value = "SELECT blp.row_number FROM blp_bulk_process_data AS blp WHERE blp.store_id = ?1 AND "
      + "blp.bulk_process_code = ?2 AND blp.status = ?3", nativeQuery = true)
  List<Integer> findRowNumberByStoreIdAndBulkProcessCodeAndStatus(String storeId, String bulkProcessCode,
      String status);

  @Query(value = "SELECT DISTINCT blp.parent_product FROM blp_bulk_process_data AS blp WHERE blp.store_id = ?1 AND "
      + "blp.bulk_process_code = ?2 AND blp.mark_for_delete = false AND blp.status = ?3", nativeQuery = true)
  List<String> getDistinctParentForBlpCode(String storeId, String bulkProcessCode, String status) throws Exception;

  @Modifying
  @Query(value = "Delete from blp_bulk_process_data where store_id = :storeId and bulk_process_code = :bulkProcessCode", nativeQuery = true)
  void deleteByBulkProcessCode(@Param("storeId") String storeId, @Param("bulkProcessCode") String bulkProcessCode);

  @Modifying
  @Query(value = "update blp_bulk_process_data set status = 'FAIL', updated_date = CURRENT_TIMESTAMP, updated_by = 'RUNDECK', "
      + "error_message = ?4 where store_id = ?1 and bulk_process_code in (?2) and "
      + "updated_date < ?3 and status in ('PENDING', 'IN_PROGRESS')", nativeQuery = true)
  void updatePendingProcesses(String storeId, List<String> bulkProcessCode, Date updatedDate, String errorMessage);

  List<BulkProcessData> findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(String storeId, String bulkProcessCode,
      List<Integer> rowNumber, String status) throws Exception;

  @Query(value = "SELECT DISTINCT blp.bulk_process_code FROM blp_bulk_process_data AS blp WHERE blp.store_id = ?1 AND "
      + "blp.bulk_process_code in (?2) AND status in ('PENDING', 'IN_PROGRESS')", nativeQuery = true)
  List<String> getPendingBulkProcessCodes(String storeId, List<String> bulkProcessCodes) throws Exception;

  @Query("SELECT NEW com.gdn.mta.bulk.dto.RowNumberParentCodeDTO (bpd.rowNumber, bpd.bulkRequestData) "
      + "FROM BulkProcessData bpd where bpd.storeId = :storeId and bpd.bulkProcessCode= :bulkProcessCode"
      + " and bpd.status = :status")
  List<RowNumberParentCodeDTO> getRowNumberAndDataByStoreIdAndBulkProcessCodeAndStatus(
      @Param("storeId") String storeId, @Param("bulkProcessCode") String bulkProcessCode,
      @Param("status") String status);

  @Modifying
  @Query(value = "UPDATE blp_bulk_process_data set status='FAIL', error_message = 'System error', "
    + "input_error_count = 1, updated_by = 'PENDING_TO_ABORTED_RUNDECK',updated_date = CURRENT_TIMESTAMP "
    + "where status in ('IN_PROGRESS', 'PENDING') and updated_date < ?1", nativeQuery = true)
  void updateStatusInPendingOrInProgressBulkProcessToAborted(Date updatedDate);

  @Modifying
  @Query(value = "UPDATE blp_bulk_process_data set status='FAIL', error_message = 'System error', "
    + "input_error_count = 1, updated_by = 'PENDING_TO_ABORTED_RUNDECK',updated_date = CURRENT_TIMESTAMP "
    + "where id = ?1", nativeQuery = true)
  void updateBulkProcessDataStatusToFailById(String id);

  Page<BulkProcessData> findByStoreIdAndBulkProcessCodeAndStatus(String storeId, String bulkProcessCode, String status,
      Pageable pageable);

  @Query("SELECT NEW com.gdn.mta.bulk.dto.BulkProcessDataDTO (bpd.id, bpd.rowNumber, bpd.bulkProcessId, "
    + "bpd.bulkProcessCode, bpd.parentProduct) FROM BulkProcessData bpd where bpd.storeId = "
    + ":storeId and bpd.bulkProcessCode= :bulkProcessCode and bpd.status = :status")
  List<BulkProcessDataDTO> getBulkProcessDataDTOByStoreIdAndBulkProcessCodeAndStatus(
    @Param("storeId") String storeId, @Param("bulkProcessCode") String bulkProcessCode, @Param("status") String status);

  @Modifying
  @Query(value = "UPDATE blp_bulk_process_data SET status='FAIL', error_message='System error' "
    + "input_error_count = 1, updated_by='ABORT_STRUCK_JOB', updated_date=CURRENT_TIMESTAMP "
    + "WHERE bulk_process_code = ?1 AND status IN (?2)", nativeQuery = true)
  void updateStatusToFailByBulkProcessCodeAndStatusIn(String bulkProcessCode, List<String> statuses);

  List<BulkProcessData> findByStoreIdAndBulkProcessCodeAndStatusAndIdentifierNotNull(String storeId, String bulkProcessCode, String status);
}