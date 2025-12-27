package com.gdn.x.productcategorybase.repository;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;

import com.gdn.x.productcategorybase.entity.ProductItem;

public interface ProductItemRepositoryCustom {

  Page<ProductItem> findByStoreIdAndUpcCodeExcludeOneItemMatches(String storeId, String upcCodeRegex, String skuCode,
      Pageable pageable);

  Page<ProductItem> findByStoreIdAndUpcCodeMatches(String storeId, String upcCodeRegex, Pageable pageable);
}
