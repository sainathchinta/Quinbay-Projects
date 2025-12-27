package com.gdn.mta.bulk.repository;

import java.util.Date;
import java.util.List;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import com.gdn.mta.bulk.entity.BulkInternalProcess;

public interface BulkInternalProcessRepository extends JpaRepository<BulkInternalProcess, String>,
    BulkInternalProcessCustomRepository {

  Page<BulkInternalProcess> findByStoreIdAndStatusAndProcessTypeAndMarkForDeleteFalse(String storeId, String status,
      String processType, Pageable pageRequest);

  Page<BulkInternalProcess> findByStoreIdAndStatusAndProcessTypeAndMarkForDeleteFalseOrderByCreatedDateAsc(String storeId, String status,
    String processType, Pageable pageRequest);

  Page<BulkInternalProcess> findByStoreIdAndProcessTypeAndCreatedDateLessThanAndStatusIn(String storeId,
      String processType, Date date, List<String> status, Pageable pageable);

  @Modifying
  @Query("UPDATE BulkInternalProcess SET status='ABORTED', updatedBy = 'PENDING_TO_ABORTED_RUNDECK',"
    + "updatedDate = CURRENT_TIMESTAMP WHERE status IN ('IN_PROGRESS', 'PUBLISHED', 'PICKED') AND "
    + "processType = :processType " + "AND updatedDate < :updatedDate")
  void updateStatusInProgressBulkInternalProcessToAborted(@Param("updatedDate") Date updatedDate,
      @Param("processType") String processType);

  @Modifying
  @Query("UPDATE BulkInternalProcess SET status = 'CANCELLED', notes = ?4, updatedBy = ?2,"
      + "updatedDate = CURRENT_TIMESTAMP WHERE storeId = ?1 AND internalProcessRequestCode = ?3")
  void bulkInternalProcessCancelRequest(String storeId, String username, String internalProcessRequestCode, String notes);

  BulkInternalProcess findByStoreIdAndInternalProcessRequestCode(String storeId, String internalProcessRequestCode);

  List<BulkInternalProcess> findByStoreIdAndCreatedByAndProcessTypeAndStatusIn(String storeId, String username, String processType, List<String> status);

  BulkInternalProcess findFirstByStoreIdAndSellerCodeAndNotesAndStatusIn(String storeId,
    String sellerCode, String notes, List<String>status);

  long countByStoreIdAndProcessTypeAndStatusIn(String storeId, String processType,
    List<String> status);

  BulkInternalProcess findFirstByStoreIdAndProcessTypeAndStatusIn(String storeId,
    String processType, List<String> status);

  long countByStoreIdAndProcessTypeAndStatusInAndCreatedByAndMarkForDeleteFalse(String storeId, String processType,
      List<String> status, String createdBy);

  long countByStoreIdAndStatusInAndCreatedByAndMarkForDeleteFalse(String storeId, List<String> status,
      String createdBy);
}
