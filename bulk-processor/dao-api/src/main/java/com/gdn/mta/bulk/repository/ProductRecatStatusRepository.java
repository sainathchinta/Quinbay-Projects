package com.gdn.mta.bulk.repository;

import java.util.List;

import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import com.gdn.mta.bulk.entity.ProductRecatStatus;

public interface ProductRecatStatusRepository extends JpaRepository<ProductRecatStatus, String> {

  @Query(value = "SELECT p.status, COUNT(p.status) FROM blp_product_recat_status AS p WHERE p.store_id = ?1 AND p.recat_request_code = ?2 GROUP BY p.status", nativeQuery = true)
  List<Object[]> countProductsByRecatRequestCode(String storeId, String recatRequestCode);

  @Query("SELECT prs from ProductRecatStatus prs where prs.storeId = :storeId and prs.status = :status order by prs.createdDate asc")
  List<ProductRecatStatus> getProductRecatStatusByStoreIdAndStatus(@Param("storeId") String storeId,
      @Param("status") String status, Pageable page);

  ProductRecatStatus findByIdAndStatus(String id, String status);

  @Query("SELECT COUNT(1) from ProductRecatStatus prs where prs.storeId = :storeId and prs.status = :status "
      + "and prs.recatRequestCode = :recatRequestCode")
  Integer findCountByStoreIdAndStatusAndRecatRequestCode(@Param("storeId") String storeId,
      @Param("status") String status, @Param("recatRequestCode") String recatRequestCode);

  List<ProductRecatStatus> findByStoreIdAndStatusAndRecatRequestCode(String storeId, String status,
      String recatRequestCode);

  @Query(value =
      "select count(1) filter (where blp_product_recat_status.status in ('PENDING', 'PUBLISHED')) as pendingProducts,"
          + " count(1) filter (where blp_product_recat_status.status = 'FINISHED') as finished,"
          + " count(1) filter (where blp_product_recat_status.status = 'FAILED') as failed,"
          + " count(1) filter (where blp_product_recat_status.validation_error = true) as validationError,"
          + " count(1) filter (where blp_product_recat_status.system_error = true) as systemError"
          + " from blp_product_recat_status where store_id = :storeId and recat_request_code = :recatRequestCode", nativeQuery = true)
  List<Object[]> findStatusCountByStoreIdAndRecatRequestCode(@Param("storeId") String storeId,
      @Param("recatRequestCode") String recatRequestCode);

}