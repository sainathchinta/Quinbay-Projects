package com.gdn.x.productcategorybase.repository;

import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;

import com.gdn.x.productcategorybase.entity.ProductItemAttributeValue;

public interface ProductItemAttributeRepository extends JpaRepository<ProductItemAttributeValue, String> {

  Long countByAttribute_IdAndValueAndMarkForDeleteFalse(String attributeId, String value);

  List<ProductItemAttributeValue> findByStoreIdAndProductItemIdIn(String storeId, List<String> productItemId);

  @Modifying
  @Query(value = "DELETE FROM pcc_product_item_attribute_value WHERE id IN ?1", nativeQuery = true)
  void deleteByIds(List<String> ids) ;

  ProductItemAttributeValue findByStoreIdAndProductItemIdAndAttributeId(String storeId,
      String productItemId, String attributeId);
}
