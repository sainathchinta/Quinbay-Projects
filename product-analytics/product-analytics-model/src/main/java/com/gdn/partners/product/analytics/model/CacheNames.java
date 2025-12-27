package com.gdn.partners.product.analytics.model;

public interface CacheNames {

  String PREFIX = "com.gdn.partners:product-analytics:";
  String FIND_DETAIL_BY_MERCHANT_CODE_CATEGORY = CacheNames.PREFIX + "findDetailByMerchantCodeAndCategoryCode";
  String FIND_DETAIL_BY_MERCHANT_CODE = CacheNames.PREFIX + "findSellerTypeByMerchantCode";
  String PRODUCT_COUNTS = CacheNames.PREFIX + "productCounts";
  String FIND_DETAIL_BY_DS_ATTRIBUTE_NAME = CacheNames.PREFIX + "dsAttributeName";

}
