package com.gdn.mta.product.repository;

import com.gdn.mta.product.entity.ProductWorkflow;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import java.util.List;

public interface ProductWorkflowRepository extends JpaRepository<ProductWorkflow, String> {

  Page<ProductWorkflow> findByStoreIdAndMarkForDeleteFalse(String storeId, Pageable pageable);

  List<ProductWorkflow> findByStoreIdAndProductIdAndMarkForDeleteFalse(String storeId, String productId);

  Page<ProductWorkflow> findByStoreIdAndProductIdAndMarkForDeleteFalse(String storeId, String productId,
      Pageable pageable);

  @Query(value = "select product_id, notes from prd_product_workflow where product_id in (:productIds) and "
      +"mark_for_delete = false and state=4", nativeQuery = true)
  List<Object[]> findByStoreIdAndProductIdIn(@Param("productIds") List<String> productIds);
}
