package com.gdn.x.productcategorybase.dto.response;

import org.apache.commons.lang3.StringUtils;

public enum UomType {
  ZC1("Pack/Set"),
  EA("Each"),
  CAR("Carton");

  private final String name;

  UomType(String name) {
    this.name = name;
  }

  public String getName() {
    return name;
  }

  public static String getNameByCode(String code) {
    for (UomType uom : UomType.values()) {
      if (uom.name().equalsIgnoreCase(code)) {
        return uom.getName();
      }
    }
    return StringUtils.EMPTY;
  }
}

