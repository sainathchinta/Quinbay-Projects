package com.gdn.x.mta.distributiontask.model.enums;

import org.apache.commons.lang3.StringUtils;

public enum ProductStateIPR {
  IN_REVIEW(0),
  WAITING_TO_GET_ACTIVATED(1),
  RELEASED(2),
  WHITELISTED(3),
  EVIDENCE_REQUESTED(4),
  EVIDENCE_SUBMITTED(5),
  SUSPENDED(6),
  REJECTED(7);

  private final int value;

  ProductStateIPR(final int value) {
    this.value = value;
  }

  public int getValue() {
    return value;
  }

  public static int getValueFromName(String state) {
    for (ProductStateIPR stateIPR : values()) {
      if (stateIPR.name().equals(state)) {
        return stateIPR.getValue();
      }
    }
    return IN_REVIEW.getValue();
  }

  public static String fromValue(int state) {
    for (ProductStateIPR stateIPR : values()) {
      if (stateIPR.getValue() == state) {
        return stateIPR.name();
      }
    }
    return StringUtils.EMPTY;
  }

  public static String getValueOrEmpty(String state) {
    for (ProductStateIPR stateIPR : values()) {
      if (stateIPR.name().equals(state)) {
        return stateIPR.name();
      }
    }
    return StringUtils.EMPTY;
  }
}
