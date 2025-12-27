package com.gdn.mta.product.service.validator;

import com.gdn.mta.product.entity.ProductBusinessPartner;

public interface ProductGdnSkuGeneratorValidator {
  void validateGenerateProductGdnSkuData(ProductBusinessPartner productBusinessPartner);

  boolean isValidGenerateProductGdnSkuData(ProductBusinessPartner productBusinessPartner);

  boolean isValidGenerateProductItemGdnSkuData(int itemNo);

  void validateGenerateProductItemGdnSkuData(int itemNo);
}
