package com.gdn.x.mta.distributiontask.model.enums;

import org.apache.commons.lang3.StringUtils;

public enum ProductSourceIPR {
  CUSTOMER_REPORT("CUSTOMER_REPORT"),
  BRAND_REPORT("BRAND_REPORT"),
  RANDOM_SAMPLE("RANDOM_SAMPLE"),
  PRICE_ANOMALY("PRICE_ANOMALY"),
  TEXT_MODEL("TEXT_MODEL");

  private final String value;

  ProductSourceIPR(final String value) {
    this.value = value;
  }

  public String getValue() {
    return value;
  }

  public static String getValueOrEmpty(String source) {
    for (ProductSourceIPR sourceIPR : values()) {
      if (sourceIPR.name().equals(source)) {
        return sourceIPR.name();
      }
    }
    return StringUtils.EMPTY;
  }
}
