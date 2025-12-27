package com.gdn.mta.product.repository;

import com.gdn.x.productcategorybase.dto.brand.BrandWipResponse;

public interface BrandWipRepository {

  BrandWipResponse findBrandWipByBrandNameAndBusinessPartnerCode(String brandName, String businessPartnerCode) throws Exception;

}
