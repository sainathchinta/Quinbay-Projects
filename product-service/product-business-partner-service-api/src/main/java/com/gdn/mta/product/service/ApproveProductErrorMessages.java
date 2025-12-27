package com.gdn.mta.product.service;

import java.text.SimpleDateFormat;

public interface ApproveProductErrorMessages {
  Integer MAXIMUM_PRODUCT_NAME_LENGTH = 150;
  String NAME_MUST_NOT_BE_BLANK = "name must not be blank";
  String NAME_LENGTH_MUST_NOT_EXCEED_MAX_ALLOWED =
      "productName length must not exceed " + MAXIMUM_PRODUCT_NAME_LENGTH + " characters";
  String DESCRIPTION_MUST_NOT_BE_BLANK = "description must not be blank";
  String LONG_DESCRIPTION_MUST_NOT_BE_BLANK = "long description must not be blank";
  String SPECIFICATION_DETAIL_MUST_NOT_BE_BLANK = "specification detail must not be blank";
  String LENGTH_MUST_NOT_BE_BLANK = "length must not be blank";
  String WIDTH_MUST_NOT_BE_BLANK = "width must not be blank";
  String HEIGHT_MUST_NOT_BE_BLANK = "height must not be blank";
  String WEIGHT_MUST_NOT_BE_BLANK = "weight must not be blank";
  String SHIPPING_WEIGHT_MUST_NOT_BE_BLANK = "shipping weight must not be blank";
  String PRODUCT_CATEGORIES_MUST_NOT_BE_BLANK = "product categories must not be blank";
  String PRODUCT_ATTRIBUTES_MUST_NOT_BE_BLANK = "product attributes must not be blank";
}
