package com.gdn.mta.bulk.repository;

import java.util.Date;
import java.util.List;
import java.util.Set;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import com.gdn.mta.bulk.dto.BulkPendingProductsDTO;
import com.gdn.mta.bulk.entity.BulkProcess;

public interface BulkProcessRepository extends JpaRepository<BulkProcess, String> {

  @Query("SELECT e from #{#entityName} e where e.storeId = ?1 and e.businessPartnerCode = ?2 and "
      + "e.bulkProcessType = ?3 and e.markForDelete = false order by e.storeId DESC, e"
      + ".businessPartnerCode DESC, e.bulkProcessType DESC, e.markForDelete DESC, e.startDate "
      + "DESC NULLS LAST")
  Page<BulkProcess> getBulkProcessNotification(String storeId, String businessPartnerCode,
      String bulkProcessType, Pageable pageable);

  BulkProcess findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(String storeId,
      String bulkProcessCode);

  BulkProcess findByStoreIdAndBulkProcessCodeAndStatusAndMarkForDeleteFalse(String storeId, String bulkProcessCode,
      String status);

  Page<BulkProcess> findByStoreIdAndBusinessPartnerCodeAndBulkProcessTypeAndCreatedByAndMarkForDeleteFalseOrderByStartDateAsc(
      String storeId, String businessPartnerCode, String bulkProcessType, String createdBy,
      Pageable pageable) throws Exception;

  Page<BulkProcess> findByStoreIdAndBusinessPartnerCodeAndCreatedByAndMarkForDeleteFalseOrderByStartDateDesc(
      String storeId, String businessPartnerCode, String createdBy, Pageable pageable);

  @Modifying
  @Query("UPDATE BulkProcess set markForDelete = true where DATE(createdDate) < DATE(?1)")
  void updateBulkProcessRecordAsMarkForDeleteTrue(String deletedDate);

  /**
   * Check if merchant has pending bulk upload requests
   *
   * @param storeId
   * @param businessPartnerCode
   * @param status
   * @return
   */
  long countByStoreIdAndBusinessPartnerCodeAndStatusInAndMarkForDeleteFalse(String storeId, String businessPartnerCode,
      List<String> status);

  /**
   * Check if merchant has pending bulk upload requests based on type
   *
   * @param storeId
   * @param businessPartnerCode
   * @param status
   * @param bulkProcessType
   * @return
   */
  long countByStoreIdAndBusinessPartnerCodeAndStatusInAndBulkProcessTypeInAndMarkForDeleteFalse(String storeId,
      String businessPartnerCode, List<String> status, Set<String> bulkProcessType);

  @Modifying
  @Query("UPDATE BulkProcess set status='ABORTED', updatedBy = 'PENDING_TO_ABORTED_RUNDECK', "
      + "updatedDate = CURRENT_TIMESTAMP where status in :statuses and updatedDate < :updatedDate")
  void updateStatusInPendingOrInProgressBulkProcessToAborted(@Param("statuses") List<String> statuses,
      @Param("updatedDate") Date updatedDate);

  /**
   *
   * @param storeId
   * @param bulkProcessType
   * @param createdBy
   * @param status
   * @return
   */
  long countByStoreIdAndBulkProcessTypeAndCreatedByAndStatusAndMarkForDeleteFalse(String storeId,
      String bulkProcessType, String createdBy, String status);

  /**
   * @param storeId
   * @return
   */
  @Query("SELECT NEW com.gdn.mta.bulk.dto.BulkPendingProductsDTO (bp.id, bp.businessPartnerCode,"
      + " bp.bulkProcessType, bp.createdBy, bp.bulkProcessCode, bp.createdDate, bp.isBulkUpdate) "
      + "FROM BulkProcess bp where bp.storeId = :storeId and bp.status='PENDING'"
      + " and bp.markForDelete = false order by bp.createdDate DESC")
  List<BulkPendingProductsDTO> getBulkPendingRequest(@Param("storeId") String storeId);

  @Query("SELECT NEW com.gdn.mta.bulk.dto.BulkPendingProductsDTO (bp.id, bp.businessPartnerCode,"
      + " bp.bulkProcessType, bp.createdBy, bp.bulkProcessCode, bp.createdDate, bp.isBulkUpdate) "
      + "FROM BulkProcess bp where bp.storeId = :storeId and bp.status = 'IN_PROGRESS'"
      + " and bp.markForDelete = false order by bp.createdDate DESC")
  List<BulkPendingProductsDTO> getBulkInProgressRequest(@Param("storeId") String storeId);


  /**
   * @param storeId
   * @param time
   * @return
   */
  @Query("SELECT NEW com.gdn.mta.bulk.dto.BulkPendingProductsDTO (bp.id, bp.businessPartnerCode,"
      + " bp.bulkProcessType, bp.createdBy, bp.bulkProcessCode, bp.createdDate, bp.isBulkUpdate) "
      + "FROM BulkProcess bp where bp.storeId = :storeId and bp.status='ABORTED'"
      + " and bp.markForDelete = false and bp.updatedDate >= :time "
      + "and bp.updatedBy = 'PENDING_TO_ABORTED_RUNDECK' order by bp.createdDate DESC")
  List<BulkPendingProductsDTO> getBulkAbortedRequest(@Param("storeId") String storeId, @Param("time") Date time);

  List<BulkProcess> findByStoreIdAndBulkProcessTypeAndStatusOrderByUpdatedDateAsc(String storeId,
      String bulkProcessType, String status, Pageable pageable);

  @Query(value = "SELECT bulk_process_code FROM blp_bulk_process where store_id = :storeId and updated_date <= "
      +":updatedDate and status in (:statusList)", nativeQuery = true)
  List<String> findByStatusAndUpdatedDate(@Param("storeId") String storeId, @Param("updatedDate") Date updatedDate,
      @Param("statusList") List<String> statusList);

  @Query(value = "SELECT bulk_process_code FROM blp_bulk_process where store_id = :storeId and updated_date <= "
      + ":endUpdatedDate and updated_date > :startUpdatedDate and status in (:statusList)", nativeQuery = true)
  List<String> findByStatusAndUpdatedDateInBetween(@Param("storeId") String storeId,
      @Param("endUpdatedDate") Date endUpdatedDate, @Param("startUpdatedDate") Date startUpdatedDate,
      @Param("statusList") List<String> statusList);

  @Modifying
  @Query("UPDATE BulkProcess set status='ABORTED', updatedBy = 'RUNDECK',"
      + "updatedDate = CURRENT_TIMESTAMP where bulkProcessCode in (?2) and updatedDate < ?1")
  void updateStatusToAbortedByBulkProcessCodes(Date updatedDate, List<String> bulkProcessCodes);

  /**
   *
   * @param storeId
   * @param businessPartnerCode
   * @param pickupPointCode
   * @return
   */
  List<BulkProcess> findByStoreIdAndBusinessPartnerCodeAndBulkProcessTypeAndNotes(String storeId,
    String businessPartnerCode, String bulkProcessType, String pickupPointCode);

  /**
   *Queries Bulk process top fetch rows with start date limit for listing
   *
   * @param storeId 10001
   * @param businessPartnerCode merchant code
   * @param bulkProcessTypesForListing ProductLevel3
   * @param startDateLimit'30 or 60
   * @param pageable page
   * @return page of bulk process
   */
  @Query("SELECT bp FROM BulkProcess bp WHERE bp.storeId = :storeId "
    + "AND bp.businessPartnerCode = :businessPartnerCode "
    + "AND bp.bulkProcessType IN :bulkProcessTypesForListing " + "AND bp.markForDelete = false "
    + "AND bp.startDate >= :startDateLimit " + "AND(:primaryIdentifier IS NULL OR bp.primaryIdentifier = :primaryIdentifier) " + "ORDER BY bp.createdDate DESC")
  Page<BulkProcess> findByStoreIdAndBusinessPartnerCodeAndPrimaryIdentifierAndBulkProcessTypeInAndMarkForDeleteFalseOrderByCreatedDateDesc(
    @Param("storeId") String storeId, @Param("businessPartnerCode") String businessPartnerCode,
    @Param("bulkProcessTypesForListing") Set<String> bulkProcessTypesForListing,
    @Param("startDateLimit") Date startDateLimit, @Param("primaryIdentifier") String primaryIdentifier, Pageable pageable);

  /**
   *Queries Bulk process to fetch rows with start date limit for listing with process code
   *
   * @param storeId 10001
   * @param bulkProcessCodes list of bulk process codes
   * @param startDateLimit'30 or 60
   * @param pageable page
   * @return page of bulk process
   */

  @Query("SELECT bp FROM BulkProcess bp WHERE bp.storeId = :storeId "
    + "AND bp.bulkProcessCode IN :bulkProcessCodes " + "AND bp.markForDelete = false "
    + "AND bp.startDate >= :startDateLimit " + "ORDER BY bp.createdDate DESC")
  List<BulkProcess> findByStoreIdAndBulkProcessCodeInAndMarkForDeleteFalseOrderByCreatedDateDesc(
    @Param("storeId") String storeId, @Param("bulkProcessCodes") List<String> bulkProcessCodes,
    @Param("startDateLimit") Date startDateLimit, Pageable pageable);


  /**
   *Queries Bulk process to fetch Process and Record Level Execution Times for Delta Times
   *
   * @param statusList list of bulk process status
   * @param createdDateLimit 30 or 60 days from current date
   * @param bulkProcessType bulk Process types
   * @return page of bulk process
   */
  @Query(value = "SELECT EXTRACT(HOUR FROM b.startDate) AS time, " + "COUNT(*) AS total_processes, "
    + "SUM(b.totalCount) AS total_records, " + "SUM(CASE WHEN b.endDate IS NULL THEN 0 "
    + "ELSE (EXTRACT(EPOCH FROM b.endDate) - EXTRACT(EPOCH FROM b.startDate)) END) AS "
    + "total_processing_time "
    + "FROM BulkProcess b " + "WHERE b.createdDate > :createdDateLimit "
    + "AND b.createdDate <= CURRENT_TIMESTAMP " + "AND b.bulkProcessType = :bulkProcessType "
    + "AND b.status IN :statusList " + "GROUP BY EXTRACT(HOUR FROM b.startDate) "
    + "ORDER BY EXTRACT(HOUR FROM b.startDate)")
  List<Object[]> getBulkProcessEstimationByBulkProcessTypeAndStatusesIn(@Param("createdDateLimit") Date createdDateLimit,
    @Param("bulkProcessType") String bulkProcessType, @Param("statusList") List<String> statusList);

  /**
   *Fetch Count of Pending Bulk Processes in Queue with Created Date Before
   *
   * @param storeId 10001
   * @param status bulk process statuses
   * @param bulkProcessType bulk Process types
   * @param createdDate Date of creation
   * @return page of bulk process
   */
  long countByStoreIdAndBulkProcessTypeAndStatusInAndCreatedDateBefore(String storeId,
    String bulkProcessType, List<String> status, Date createdDate);

  @Modifying
  @Query(
    "UPDATE BulkProcess SET status='ABORTED', updatedBy='PENDING_TO_ABORTED_RUNDECK', updatedDate=CURRENT_TIMESTAMP "
      + "WHERE status IN :statuses AND updatedDate < :updatedDate AND bulkProcessType = :bulkProcessType")
  void updateStatusInPendingOrInProgressByBulkProcessTypeAndTimeToAborted(
    @Param("bulkProcessType") String bulkProcessType, @Param("updatedDate") Date updatedDate,
    @Param("statuses") List<String> statuses);

  Page<String> findBulkProcessCodeByStoreIdAndBulkProcessTypeAndStatusInAndUpdatedDateBeforeOrderByUpdatedDateAsc(
    String storeId, String bulkProcessType, List<String> status, Date updatedDate,
    Pageable pageable);

}
