package com.gdn.x.productcategorybase.repository;

import java.util.List;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import com.gdn.x.productcategorybase.entity.ProductCategory;

public interface ProductCategoryRepository extends JpaRepository<ProductCategory, String> {

  List<ProductCategory> findByStoreIdAndCategoryIdAndMarkForDeleteFalse(String storeId, String categoryId);
  
  List<ProductCategory> findByStoreIdAndProductIdAndMarkForDeleteFalse(String storeId, String productId);

  ProductCategory findByStoreIdAndIdAndMarkForDeleteFalse(String storeId, String id);

  Page<ProductCategory> findByStoreIdAndMarkForDeleteFalse(String storeId, Pageable pageable);

  @Query(value = "select product_id from pcc_product_category where product_id in (:productIds) and "
      + "category_id in (:categoryIds) and mark_for_delete = false", nativeQuery = true)
  List<String> findByProductIdInAndCategoryIdInAndMarkForDeleteFalse(@Param("productIds") List<String> productIds,
      @Param("categoryIds") List<String> categoryIds);

  @Query(value = "select category_id, count(category_id) from pcc_product_category where product_id in (:productIds) and mark_for_delete = false group by category_id", nativeQuery = true)
  List<Object[]> findCategoryCountByProductIdInAndMarkForDeleteFalse(@Param("productIds") List<String> productIds);

  List<ProductCategory> findByStoreIdAndProductId(String storeId, String productId);

  @Modifying
  @Query(value = "DELETE FROM pcc_product_category WHERE id IN ?1", nativeQuery = true)
  void deleteByIds(List<String> ids) ;
}
