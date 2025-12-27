package com.gdn.x.productcategorybase.repository;

import java.util.List;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import com.gdn.x.productcategorybase.entity.Attribute;
import com.gdn.x.productcategorybase.entity.Product;
import com.gdn.x.productcategorybase.entity.ProductAttribute;

public interface ProductAttributeRepository extends JpaRepository<ProductAttribute, String> {

  ProductAttribute findByIdAndMarkForDeleteFalse(String id);

  ProductAttribute findByStoreIdAndIdAndMarkForDeleteFalse(String storeId, String id);

  List<ProductAttribute> findByStoreIdAndMarkForDeleteFalse(String storeId);

  Page<ProductAttribute> findByStoreIdAndMarkForDeleteFalse(String storeId, Pageable pageable);

  List<ProductAttribute> findByStoreIdAndProductId(String storeId, String productId);

  ProductAttribute findByStoreIdAndProductIdAndAttributeIdAndMarkForDeleteFalse(String storeId, String productId,
      String attributeId);

  @Modifying
  @Query(value = "DELETE FROM pcc_product_attribute WHERE id IN ?1", nativeQuery = true)
  void deleteByIds(List<String> ids) ;
}
