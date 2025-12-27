package com.gdn.x.product.rest.web.helper.api;

import com.fasterxml.jackson.core.JsonFactory;

public class JsonFactoryHelper {

  private final boolean enableCanonicalize;

  private final boolean enableIntern;

  public JsonFactoryHelper(boolean disableCanonicalize, boolean disableIntern) {
    this.enableCanonicalize = disableCanonicalize;
    this.enableIntern = disableIntern;
  }

  public JsonFactory getNewInstance() {
    JsonFactory jsonFactory = new JsonFactory();
    if (enableCanonicalize) {
      jsonFactory.disable(JsonFactory.Feature.CANONICALIZE_FIELD_NAMES);
    }
    if (enableIntern) {
      jsonFactory.disable(JsonFactory.Feature.INTERN_FIELD_NAMES);
    }
    return jsonFactory;
  }

}
