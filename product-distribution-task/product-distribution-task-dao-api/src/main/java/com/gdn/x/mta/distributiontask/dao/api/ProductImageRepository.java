package com.gdn.x.mta.distributiontask.dao.api;

import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import com.gdn.x.mta.distributiontask.model.ProductImage;

public interface ProductImageRepository extends JpaRepository<ProductImage, String> {

  @Modifying
  @Query(value = "update ProductImage pi set pi.markForDelete=true, pi.updatedBy = :updatedBy, pi.updatedDate = CURRENT_TIMESTAMP "
      + "where pi.product.id = :productId")
  void deleteByProductId(@Param("productId") String productId, @Param("updatedBy") String updatedBy);

  @Modifying
  @Query(value = "delete from pdt_product_image pi where pi.product IN (:productIdList)", nativeQuery = true)
  void deleteByProductIds(@Param("productIdList") List<String> productIdList);

  @Query(value = "select * from pdt_product_image where product = ?1", nativeQuery = true)
  List<ProductImage> findByProductId(String productId);

  @Modifying
  @Query(value = "UPDATE pdt_product_image SET location_path = :newLocationPath WHERE product = :productId and location_path = :oldLocationPath", nativeQuery = true)
  void updateLocationPathByProductId(@Param("newLocationPath") String newLocationPath,
      @Param("productId") String productId, @Param("oldLocationPath") String oldLocationPath);

}
