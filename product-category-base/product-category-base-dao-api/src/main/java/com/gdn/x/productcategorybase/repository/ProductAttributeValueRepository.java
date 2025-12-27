package com.gdn.x.productcategorybase.repository;

import java.util.List;
import java.util.Set;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;

import com.gdn.x.productcategorybase.entity.AllowedAttributeValue;
import com.gdn.x.productcategorybase.entity.ProductAttributeValue;

public interface ProductAttributeValueRepository extends JpaRepository<ProductAttributeValue, String> {
  List<ProductAttributeValue> findByStoreIdAndAllowedAttributeValue(String storeId,
      AllowedAttributeValue allowedAttributeValue);

  ProductAttributeValue findByStoreIdAndIdAndMarkForDeleteFalse(String storeId, String id);

  List<ProductAttributeValue> findByStoreIdAndMarkForDeleteFalse(String storeId);

  Page<ProductAttributeValue> findByStoreIdAndMarkForDeleteFalse(String storeId, Pageable pageable);

  List<ProductAttributeValue> findByStoreIdAndProductAttributeIdInAndMarkForDeleteFalse(
      String storeId, Set<String> productAttributeIds);

  List<ProductAttributeValue> findByStoreIdAndProductAttributeIdIn(String storeId, Set<String> productAttributeIds);

  @Modifying
  @Query(value = "DELETE FROM pcc_product_attribute_value WHERE id IN ?1", nativeQuery = true)
  void deleteByIds(List<String> ids) ;
}
