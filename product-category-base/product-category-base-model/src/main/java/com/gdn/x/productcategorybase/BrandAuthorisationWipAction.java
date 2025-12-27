package com.gdn.x.productcategorybase;

import org.apache.commons.lang3.StringUtils;

public enum BrandAuthorisationWipAction {
  APPROVE, NEED_REVISION, REJECT;

  public static String getValueOrEmpty(String state) {
    for (BrandAuthorisationWipAction brandAuthorisationWipAction : values()) {
      if (brandAuthorisationWipAction.name().equals(state)) {
        return brandAuthorisationWipAction.name();
      }
    }
    return StringUtils.EMPTY;
  }
}
