package com.gdn.x.productcategorybase.repository;

import java.util.List;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import com.gdn.x.productcategorybase.entity.CategoryShipping;

public interface CategoryShippingRepository extends JpaRepository<CategoryShipping, String> {

  @Query(value = "SELECT cs FROM CategoryShipping cs WHERE cs.storeId = :storeId AND cs.categoryCode = :categoryCode AND cs.markForDelete = false")
  List<CategoryShipping> findByStoreIdAndCategoryCodeAndMarkForDeleteFalse(@Param("storeId") String storeId, @Param("categoryCode") String categoryCode);

  Page<CategoryShipping> findByStoreIdAndCategoryCodeAndMarkForDeleteFalse(String storeId, String categoryCode,
      Pageable pageable);

  CategoryShipping findByStoreIdAndId(String storeId, String id);

  List<CategoryShipping> findByStoreIdAndMarkForDeleteFalse(String storeId);

  Page<CategoryShipping> findByStoreIdAndMarkForDeleteFalse(String storeId, Pageable pageable);

  List<CategoryShipping> findByStoreIdAndShippingCodeAndMarkForDeleteFalse(String storeId, String shippingCode);

  Page<CategoryShipping> findByStoreIdAndShippingCodeAndMarkForDeleteFalse(String storeId, String shippingCode,
      Pageable pageable);

}
