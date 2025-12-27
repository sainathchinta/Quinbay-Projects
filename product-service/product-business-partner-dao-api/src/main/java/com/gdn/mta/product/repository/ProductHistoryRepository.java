package com.gdn.mta.product.repository;

import java.util.List;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import com.gdn.mta.product.entity.ProductHistory;


public interface ProductHistoryRepository extends JpaRepository<ProductHistory, String> {

  Page<ProductHistory> findByStoreIdAndMarkForDeleteFalse(String storeId, Pageable pageable);

  Page<ProductHistory> findByStoreIdAndProductIdAndMarkForDeleteFalseOrderByCreatedDateDesc(
      String storeId, String productId, Pageable pageable);

  List<ProductHistory> findByStoreIdAndProductIdAndMarkForDeleteFalse(String storeId,
      String productId);

  ProductHistory findTop1ByStoreIdAndProductIdAndMarkForDeleteFalseAndDescriptionOrderByCreatedDateDesc(String storeId,
      String productId, String description);

  ProductHistory findTop1ByStoreIdAndProductIdAndMarkForDeleteFalseOrderByCreatedDateDesc(String storeId,
      String productId);

  @Modifying(clearAutomatically = true)
  @Query("update ProductHistory ph set ph.productId = :newProductId WHERE ph.productId = :oldProductId and ph.description ="
             + " :description")
  int updateProductForResubmittedProducts(@Param("oldProductId") String oldProductId,
      @Param("newProductId") String newProductId, @Param("description") String description);

  long deleteByStoreIdAndProductId(String storeId, String productId);

}
