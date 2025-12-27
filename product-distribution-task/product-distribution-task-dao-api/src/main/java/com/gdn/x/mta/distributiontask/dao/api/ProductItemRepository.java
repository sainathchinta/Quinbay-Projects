package com.gdn.x.mta.distributiontask.dao.api;

import java.util.List;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Slice;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import com.gdn.x.mta.distributiontask.model.ProductItem;

public interface ProductItemRepository extends JpaRepository<ProductItem, String> {

  @Modifying
  @Query(
      value = "update ProductItem pi set pi.markForDelete = true, pi.updatedBy = :updatedBy, pi.updatedDate = CURRENT_TIMESTAMP where pi.product.id = :productId")
  void deleteByProductId(@Param("productId") String productId, @Param("updatedBy") String updatedBy);

  @Query(value = "select pi.id from ProductItem pi where pi.product.id in (:productIds) order by pi.id asc")
  Slice<String> findByProductIds(@Param("productIds") List<String> productIds, Pageable pageable);

  @Modifying
  @Query(value = "delete from pdt_product_item pi where pi.product in (:productIdList)", nativeQuery = true)
  void deleteByProductIds(@Param("productIdList") List<String> productIdList);

  @Query(value = "SELECT id FROM pdt_product_item WHERE product = :productId AND mark_for_delete = FALSE", nativeQuery = true)
  List<String> getIdsByProductIdAndMarkForDeleteFalse(@Param("productId") String productId);

  @Query("SELECT pi.id FROM ProductItem pi WHERE pi.product.id = ?1")
  List<String> findIdByProduct(String productId);
}
