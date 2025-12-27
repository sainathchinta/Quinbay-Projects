package com.gdn.x.productcategorybase.repository;

import java.util.List;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import com.gdn.x.productcategorybase.entity.Attribute;
import com.gdn.x.productcategorybase.entity.CategoryAttribute;

public interface CategoryAttributeRepository extends JpaRepository<CategoryAttribute, String> {

  CategoryAttribute findByIdAndMarkForDeleteFalse(String id);

  CategoryAttribute findByStoreIdAndIdAndMarkForDeleteFalse(String storeId, String id);

  @Query(value = "select pca.category_id from pcc_category_attribute pca where pca.store_id = "
      + ":storeId AND pca.attribute_id = :attributeId", nativeQuery = true)
  List<String> findCategoryIdByStoreIdAndAttributeIdAndMarkForDeleteFalse(
      @Param("storeId") String storeId, @Param("attributeId") String attributeId);

  List<CategoryAttribute> findByStoreIdAndAttribute(String storeId, Attribute attribute);

  List<CategoryAttribute> findByStoreIdAndCategoryId(String storeId, String categoryId);

  List<CategoryAttribute> findByStoreIdAndCategoryIdAndMarkForDeleteFalse(String storeId, String categoryId);
}
