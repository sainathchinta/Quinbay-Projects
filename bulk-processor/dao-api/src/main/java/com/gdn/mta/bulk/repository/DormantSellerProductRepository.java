package com.gdn.mta.bulk.repository;

import com.gdn.mta.bulk.entity.DormantSellerProduct;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import java.util.Date;
import java.util.List;

public interface DormantSellerProductRepository extends JpaRepository<DormantSellerProduct, String> {

  Page<DormantSellerProduct> findByStoreIdAndStatusAndProcessTypeAndMarkForDeleteFalse(
    String storeId, String status, String processType, Pageable pageable);

  DormantSellerProduct findByItemSkuAndStatusAndProcessTypeAndProductStatusAndMarkForDeleteFalse(String itemSku,
      String name, String processType, String productStatus);

  DormantSellerProduct findByItemSkuAndStatusAndProcessTypeAndProductStatusAndDormantSellerEventIdAndMarkForDeleteFalse(
      String itemSku, String name, String processType, String productStatus, String dormantSellerEventId);

  List<DormantSellerProduct> findByDormantSellerEventIdAndMarkForDeleteFalse(String dormantSellerEventId);

  List<DormantSellerProduct> findByItemSkuIn(List<String> itemSkuList);

  @Query(value = "select p from DormantSellerProduct p where "
      + "p.markForDelete = true AND p.updatedDate <= :updateDate order by p.updatedDate desc, p.id asc")
  Page<DormantSellerProduct> findByUpdatedDateAndMarkForDeleteTrue(@Param("updateDate") Date updateDate,
      Pageable pageable);

  List<DormantSellerProduct> findByStoreIdAndStatusInAndProcessTypeInAndMarkForDeleteFalseAndUpdatedDateBefore(
      String storeId, List<String> statuses, List<String> processTypes, Date endDate);
}
