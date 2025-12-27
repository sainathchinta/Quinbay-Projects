package com.gdn.partners.product.analytics.model;

public final class ProductAttributeFeedbackFields {

  private ProductAttributeFeedbackFields() {
    throw new UnsupportedOperationException(Constants.UTILITY_CLASS_ERROR);
  }

  public static final String COLLECTION_NAME = "product_attribute_feedback";
  public static final String PRODUCT_CODE = "product_code";
  public static final String ATTRIBUTE_NAME = "attribute_name";
  public static final String PREVIOUS_VALUE = "previous_value";
  public static final String CURRENT_VALUE = "current_value";
}
