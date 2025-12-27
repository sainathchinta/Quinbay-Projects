package com.gdn.mta.bulk.repository;

import com.gdn.mta.bulk.entity.BulkProcessDataEstimation;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;

import java.util.Date;
import java.util.List;

public interface BulkProcessDataEstimationRepository extends JpaRepository<BulkProcessDataEstimation,
  String> {

  String QUERY_UPDATE_ESTIMATIONS_AT_RECORD_LEVEL =
    "UPDATE BulkProcessDataEstimation SET processLevelFetch = ?4, " + "deltaTimeEstimations = ?2, "
      + "lastFetchTime = ?3, " + "markForDelete = false " + "WHERE processType = ?1";

  @Modifying
  @Query(value = QUERY_UPDATE_ESTIMATIONS_AT_RECORD_LEVEL)
  void updateByProcessType(String processType, String deltaTimeEstimations,
    Date lastFetchTime, boolean processLevelFetch);

  long countByProcessTypeAndProcessLevelFetch(String processType, boolean processLevelFetch);

  BulkProcessDataEstimation findFirstByProcessTypeAndProcessLevelFetch(String processType,
    boolean processLevelFetch);

  List<BulkProcessDataEstimation> findByProcessTypeInAndMarkForDeleteFalse(List<String> processTypes);
}
