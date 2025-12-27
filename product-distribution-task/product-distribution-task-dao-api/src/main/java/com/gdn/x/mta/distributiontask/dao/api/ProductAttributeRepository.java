package com.gdn.x.mta.distributiontask.dao.api;

import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import com.gdn.x.mta.distributiontask.model.ProductAttribute;

public interface ProductAttributeRepository extends JpaRepository<ProductAttribute, String> {

  @Modifying
  @Query(value = "update ProductAttribute pa set pa.markForDelete=true, pa.updatedDate = CURRENT_TIMESTAMP, pa.updatedBy = :updatedBy  where pa.product.id = :productId")
  void deleteByProductId(@Param("productId") String productId, @Param("updatedBy") String updatedBy);

  @Modifying
  @Query(value = "delete from pdt_product_attribute pa where pa.product IN (:productIdList)", nativeQuery = true)
  void deleteByProductIds(@Param("productIdList") List<String> productIdList);

  @Modifying
  @Query(value = "UPDATE pdt_product_attribute"
      + " SET updated_date = now(), updated_by = 'system', value = :attributeValue"
      + " WHERE product IN (:productIdList) AND name =:attributeName", nativeQuery = true)
  void updateValueByNameAndProductIds(@Param("attributeName") String attributeName,
      @Param("attributeValue") String attributeValue, @Param("productIdList") List<String> productIdList);
}
