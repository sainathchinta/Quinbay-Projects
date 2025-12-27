package com.gdn.mta.product.repository;

import java.util.Date;
import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import com.gdn.mta.product.entity.LogAuditTrailUpdatedProductBackup;

public interface AuditTrailUpdateProductBackupRepository
    extends JpaRepository<LogAuditTrailUpdatedProductBackup, String> {

  @Modifying
  @Query(value = "insert into PRD_LOG_AUDIT_TRAIL_UPDATED_PRODUCT_BACKUP (select * from "
      + "PRD_UPDATED_PRODUCT_HISTORY where access_time <= :accessTime)", nativeQuery = true)
  void saveBackup(@Param("accessTime") Date accessTime);

  @Modifying
  @Query(value = "insert into PRD_LOG_AUDIT_TRAIL_UPDATED_PRODUCT_BACKUP (select * from "
      + "PRD_UPDATED_PRODUCT_HISTORY where audit_trail_id in (:auditTrailIds))", nativeQuery = true)
  void saveBackupByAuditTrailIds(@Param("auditTrailIds") List<String> auditTrailIds);

}
