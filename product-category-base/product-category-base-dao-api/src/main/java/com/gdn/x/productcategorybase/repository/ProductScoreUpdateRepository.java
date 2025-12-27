package com.gdn.x.productcategorybase.repository;

import java.util.List;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import com.gdn.x.productcategorybase.entity.ProductScoreUpdate;

public interface ProductScoreUpdateRepository extends JpaRepository<ProductScoreUpdate, String> {

  @Query(value = "select p.productCode from ProductScoreUpdate p where p.markForDelete = false and p.updated = false order by p.productCode desc")
  Page<String> findByMarkForDeleteFalseAndUpdatedFalse(Pageable pageable);

  @Modifying
  @Query(value = "update PCC_PRODUCT_SCORE_UPDATE set IS_UPDATED = true, updated_date = CURRENT_TIMESTAMP, updated_by = 'RUNDECK' "
      + "where product_code in (:productCodes)", nativeQuery = true)
  void updateProducts(@Param("productCodes") List<String> productCodes);
}
