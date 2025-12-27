package com.gdn.x.productcategorybase.repository;

import java.util.Date;
import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;

import com.gdn.x.productcategorybase.entity.ProductArchive;

public interface ProductArchiveRepository extends JpaRepository<ProductArchive, String> {

  @Query(value = "SELECT product_id FROM pcc_product_archive WHERE created_date < ?1 LIMIT ?2", nativeQuery = true)
  List<String> findIdByCreatedDateLessThan(Date date, int batchSize);

  @Modifying
  @Query(value = "DELETE FROM pcc_product_archive WHERE product_id IN ?1 ", nativeQuery = true)
  void deleteByProductIdIn(List<String> productIds);

}
