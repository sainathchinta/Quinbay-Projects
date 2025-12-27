package com.gdn.x.productcategorybase.repository;

import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;

import com.gdn.x.productcategorybase.entity.ProductImageArchive;

public interface ProductImageArchiveRepository extends JpaRepository<ProductImageArchive, String> {

  @Modifying
  @Query(value = "DELETE FROM pcc_product_images_archive WHERE product_id IN ?1 ", nativeQuery = true)
  void deleteByProductIdIn(List<String> productIds);
}
