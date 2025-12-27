package com.gdn.mta.bulk.repository;

import java.util.Date;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;

import com.gdn.mta.bulk.entity.UnifiedBulkDownloadEvent;

public interface UnifiedBulkDownloadRepository extends JpaRepository<UnifiedBulkDownloadEvent, String> {

  UnifiedBulkDownloadEvent findByStoreIdAndBusinessPartnerCode(String storeId, String businessPartnerCode);

  @Modifying
  @Query("UPDATE UnifiedBulkDownloadEvent set downloadStatus='ABORTED', updatedBy = ?1, updatedDate = CURRENT_TIMESTAMP "
      + "where downloadStatus='IN_PROGRESS' and updatedDate < ?2")
  void updatePendingDownloadProcesses(String updatedBy, Date updatedDate);
}