package com.gdn.x.mta.distributiontask.dao.api;

import java.util.Date;
import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import com.gdn.x.mta.distributiontask.model.ProductReviewer;

@Repository
public interface ProductReviewerRepository extends JpaRepository<ProductReviewer, String> {

  List<ProductReviewer> findByStoreIdAndProductCodeIn(String storeId,
    List<String> productCodes);

  ProductReviewer findByStoreIdAndProductCode(String storeId, String productCode);

  ProductReviewer findByProductCode(String productCode);

  @Modifying
  @Query(value =
      "update ProductReviewer p set approverAssignee = :approverAssignee, assignedDate = :assignedDate "
          + "where productCode in (:productCode)")
  void updateProductAssignment(@Param("approverAssignee") String approverAssignee,
      @Param("assignedDate") Date assignedDate, @Param("productCode") List<String> productCodes);

  @Modifying
  @Query(value = "Delete from pdt_product_reviewer pr where pr.product_code in (:productCodes)", nativeQuery = true)
  void deleteByProductCodeIn(@Param("productCodes") List<String> productCodeList);
}
