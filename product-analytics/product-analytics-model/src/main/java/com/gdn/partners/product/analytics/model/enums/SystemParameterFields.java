package com.gdn.partners.product.analytics.model.enums;

import com.gdn.partners.product.analytics.model.Constants;

public class SystemParameterFields {

  public static final String COLLECTION_NAME = "system_parameter";
  public static final String VARIABLE = "variable";
  public static final String VALUE = "value";
  public static final String DESCRIPTION = "description";

  private SystemParameterFields() {
    throw new UnsupportedOperationException(Constants.UTILITY_CLASS_ERROR);
  }
}
