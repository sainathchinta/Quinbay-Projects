package com.gdn.partners.bulk.util;

import java.util.Arrays;
import java.util.Collection;

public interface BrandAuthorisationConstant {

  //Excel Header
  String BRAND_CODE = "brand code";
  String BRAND_NAME = "brand name";
  String SELLER_CODE = "seller code";
  String SELLER_NAME = "seller name";

  Collection<String> TEMPLATE_HEADER =
      Arrays.asList(BRAND_CODE, BRAND_NAME, SELLER_CODE, SELLER_NAME);

}
