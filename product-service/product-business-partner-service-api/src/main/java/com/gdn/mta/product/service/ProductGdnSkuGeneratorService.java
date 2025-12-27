package com.gdn.mta.product.service;

import com.gdn.mta.product.entity.ProductBusinessPartner;

public interface ProductGdnSkuGeneratorService {

  void generateGdnSkuOnProduct(ProductBusinessPartner productBusinessPartner, boolean isMppFlow);

  String convertToGeneratedGdnSkus(ProductBusinessPartner productBusinessPartner);
}
