package com.gdn.x.productcategorybase.repository;

import com.gdn.x.productcategorybase.entity.WholesalePriceConfiguration;

import org.springframework.data.jpa.repository.JpaRepository;

public interface CategoryWholesaleConfigRepository extends JpaRepository<WholesalePriceConfiguration, String> {

  WholesalePriceConfiguration findByStoreIdAndCategoryId(String storeId, String categoryId);
}
