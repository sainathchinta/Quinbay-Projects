package com.gdn.x.mta.distributiontask.dao.api;

import java.util.List;

import com.gdn.x.mta.distributiontask.model.ProductItemAttribute;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

/**
 * Created by virajjasani on 23/09/16.
 */
public interface ProductItemAttributeRepository
    extends JpaRepository<ProductItemAttribute, String> {

  @Modifying
  @Query(value = "delete from pdt_product_item_attribute pii where pii.product_item IN (:productItemIdList)", nativeQuery = true)
  void deleteByProductItemIds(@Param("productItemIdList") List<String> productItemIdList);

  @Modifying
  @Query(value = "UPDATE pdt_product_item_attribute "
      + " SET updated_date = now(), updated_by = 'system', value = :attributeValue"
      + " WHERE product_item IN (:productItemIdList) AND name =:attributeName", nativeQuery = true)
  void updateValueByNameAndProductIds(@Param("attributeName") String attributeName,
      @Param("attributeValue") String attributeValue, @Param("productItemIdList") List<String> productItemIdList);
}
