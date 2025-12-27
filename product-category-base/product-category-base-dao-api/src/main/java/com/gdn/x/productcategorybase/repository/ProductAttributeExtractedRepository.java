package com.gdn.x.productcategorybase.repository;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;

import com.gdn.x.productcategorybase.entity.ExtractionStatus;
import com.gdn.x.productcategorybase.entity.ProductAttributeExtracted;

public interface ProductAttributeExtractedRepository extends JpaRepository<ProductAttributeExtracted, String> {

  ProductAttributeExtracted findByStoreIdAndProductCodeAndMarkForDeleteFalse(String storeId, String productCode);

  Page<ProductAttributeExtracted> findByStoreIdAndStatusAndMarkForDeleteFalse(String storeId, ExtractionStatus status,
      Pageable pageable);

  void deleteByProductCode(String productCode);
}
