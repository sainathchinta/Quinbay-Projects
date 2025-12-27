package com.gdn.mta.product.repository;

import com.gdn.x.productcategorybase.dto.brand.BrandResponse;

public interface BrandRepository {

  BrandResponse findBrandByBrandName(String brandName) throws Exception;
}
