package com.gdn.x.productcategorybase.repository;

import java.util.Date;
import java.util.List;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import com.gdn.x.productcategorybase.entity.ProductImageCleanup;

public interface ProductImageCleanupRepository extends JpaRepository<ProductImageCleanup, String> {

  @Query("select locationPath FROM ProductImageCleanup where updatedDate < :updatedDate")
  Page<String> findLocationPathByUpdatedDateLessThan(@Param("updatedDate") Date updatedDate, Pageable pageable);

  @Modifying(clearAutomatically = true)
  @Query(value = "DELETE FROM pcc_product_images_cleanup where location_path in (:locationPaths)", nativeQuery = true)
  void deleteByLocationPath(@Param("locationPaths") List<String> locationPaths);

  @Query("select locationPath FROM ProductImageCleanup where productCode = :productCode")
  List<String> findLocationPathByProductCode(@Param("productCode") String productCode);
}
