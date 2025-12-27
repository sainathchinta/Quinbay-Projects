package com.gdn.x.productcategorybase.repository;

import com.gdn.x.productcategorybase.entity.Dimension;
import com.gdn.x.productcategorybase.entity.DimensionMapping;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.List;

public interface DimensionMappingRepository extends JpaRepository<DimensionMapping, String> {

  Page<DimensionMapping> findByStoreIdAndAttributeCodeAndMarkForDeleteFalse(String storeId,
      String attributeCode, Pageable pageable);

  DimensionMapping findByStoreIdAndAttributeCodeAndDimensionAndMarkForDeleteFalse(String storeId,
      String attributeCode, Dimension dimension);

  List<DimensionMapping> findByStoreIdAndAttributeCodeAndMarkForDeleteFalse(String storeId,
    String attributeCode);
}
