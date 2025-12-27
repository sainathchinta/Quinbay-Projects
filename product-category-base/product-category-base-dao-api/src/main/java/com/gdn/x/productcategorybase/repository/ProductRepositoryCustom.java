package com.gdn.x.productcategorybase.repository;

import java.util.List;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;

import com.gdn.x.productcategorybase.entity.Product;

public interface ProductRepositoryCustom {

  Page<Product> findByStoreIdAndCategoriesCode(String storeId, List<String> categoriesCode, Pageable pageable);

}
