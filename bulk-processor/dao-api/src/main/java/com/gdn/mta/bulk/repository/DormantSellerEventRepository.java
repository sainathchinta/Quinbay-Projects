package com.gdn.mta.bulk.repository;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import com.gdn.mta.bulk.entity.DormantSellerEvent;

import java.util.Date;
import java.util.List;

public interface DormantSellerEventRepository extends JpaRepository<DormantSellerEvent, String> {

  List<DormantSellerEvent> findByStoreIdAndStatusAndMarkForDeleteFalse(String storeId, String status);

  Page<DormantSellerEvent> findByStoreIdAndStatusAndMarkForDeleteFalse(String storeId, String status, Pageable pageable);

  Page<DormantSellerEvent> findByStoreIdAndStatusAndProcessTypeAndMarkForDeleteFalse(String storeId,
    String status, String processType, Pageable pageable);

  List<DormantSellerEvent> findByStoreIdAndBusinessPartnerCodeIn(String storeId, List<String> businessPartnerCodes);

  DormantSellerEvent findFirstByBusinessPartnerCodeAndProcessType(String businessPartnerCode, String processType);

  List<DormantSellerEvent> findByStoreIdAndBusinessPartnerCodeAndProcessTypeAndMarkForDeleteFalse(String storeId,
      String businessPartnerCode, String processType);

  @Query(value = "select d from DormantSellerEvent d where "
      + "storeId = :storeId AND status in :states and updatedDate <= :updateDate and markForDelete = false "
      + "order by updatedDate desc")
  Page<DormantSellerEvent> findByStoreIdAndStatesInAndUpdatedDateAndMarkForDeleteFalse(
      @Param("storeId") String storeId, @Param("states") List<String> states, @Param("updateDate") Date updateDate,
      Pageable pageable);

  @Query(value = "select d from DormantSellerEvent d where "
      + "storeId = :storeId AND status in :states and updatedDate >= :updateDate and markForDelete = false "
      + "order by updatedDate desc")
  Page<DormantSellerEvent> findByStoreIdAndStatesInAndUpdatedDateAfterAndMarkForDeleteFalse(
      @Param("storeId") String storeId, @Param("states") List<String> states, @Param("updateDate") Date updateDate,
      Pageable pageable);
}
