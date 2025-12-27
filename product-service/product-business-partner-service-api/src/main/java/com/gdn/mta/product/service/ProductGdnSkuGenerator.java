package com.gdn.mta.product.service;


public interface ProductGdnSkuGenerator {
  String generateProductGdnSku(String businessPartnerCode);

  String generateProductItemGdnSku(String productSku, int itemNo);
}
