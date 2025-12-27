package com.gdn.mta.bulk.repository;

import java.util.Collection;
import java.util.Date;
import java.util.List;
import java.util.Set;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import com.gdn.mta.bulk.entity.BulkDownloadEntity;

/**
 * Created by virajjasani on 31/08/16.
 */
public interface BulkDownloadAuditRepository extends JpaRepository<BulkDownloadEntity, String> {
  BulkDownloadEntity findByRequestId(String requestId) throws Exception;

  BulkDownloadEntity findByRequestIdAndStatus(String requestId, String status) throws Exception;

  List<BulkDownloadEntity> findByEntityTypeAndBusinessPartnerCodeAndStatusIn(String entityType,
      String businessPartnerCode, List<String> statusList) throws Exception;

  Integer countByEntityTypeAndBusinessPartnerCodeAndStatusInAndCreatedByAndMarkForDeleteFalse(String entityType,
      String businessPartnerCode, List<String> status, String createdBy) throws Exception;

  @Modifying
  @Query("UPDATE BulkDownloadEntity set status='ABORTED' where status = ?2"
      + " AND entityType = ?3 AND createdDate < ?1")
  void updateStatusInProgressBulkDownloadEntityToAborted(Date pendingToAbortDate, String status, String entityType);

  List<BulkDownloadEntity> findByStatusOrderByCreatedDateAsc(String status, Pageable pageRequest);

  @Query(
    "SELECT bde FROM BulkDownloadEntity bde WHERE bde.businessPartnerCode = :businessPartnerCode "
      + "AND bde.entityType IN :entityTypes "
      + "AND bde.markForDelete = false " + "AND bde.createdDate >= :createdDateLimit "
      + "AND (:primaryIdentifier IS NULL OR bde.primaryIdentifier = :primaryIdentifier) "
      + "ORDER BY bde.createdDate DESC")
  Page<BulkDownloadEntity> findByBusinessPartnerCodeAndPrimaryIdentifierAndEntityTypeInAndMarkForDeleteFalseAndCreatedDateLimit(
    @Param("businessPartnerCode") String businessPartnerCode, @Param("entityTypes") Set<String> entityTypes,
    @Param("createdDateLimit") Date createdDateLimit, @Param("primaryIdentifier") String primaryIdentifier, Pageable pageable);

  BulkDownloadEntity findFirstByEntityTypeAndBusinessPartnerCodeAndStatusIn(String entityType,
      String businessPartnerCode, Collection<String> status);

  BulkDownloadEntity findFirstByEntityTypeAndCreatedByAndStatusIn(String entityType,
      String createdBy, Collection<String> status);
}
