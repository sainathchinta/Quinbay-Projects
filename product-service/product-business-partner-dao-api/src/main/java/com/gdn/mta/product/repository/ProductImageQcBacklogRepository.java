package com.gdn.mta.product.repository;

import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import com.gdn.mta.product.entity.ProductImageQcBacklog;

public interface ProductImageQcBacklogRepository extends JpaRepository<ProductImageQcBacklog, String> {

  ProductImageQcBacklog findByStoreIdAndProductCodeAndStatus(String storeId, String productCode, String status);

  @Query(value = "SELECT * FROM prd_product_image_qc_backlog piqc WHERE piqc.store_id = :storeId AND"
      + " piqc.status = :status order by :orderProductsBy desc limit :limit", nativeQuery = true)
  List<ProductImageQcBacklog> findByStoreIdAndStatus(@Param("storeId") String storeId, @Param("status") String status,
      @Param("orderProductsBy") String orderProductsBy, @Param("limit") int limit);

}