package com.gdn.mta.product.service;

import org.apache.commons.lang3.StringUtils;

public enum Uom {
  ZC1("Pack/Set"),
  EA("Each"),
  CAR("Carton");

  private final String name;

  Uom(String name) {
    this.name = name;
  }

  public String getName() {
    return name;
  }

  public static String getNameByCode(String code) {
    for (Uom uom : Uom.values()) {
      if (uom.name().equalsIgnoreCase(code)) {
        return uom.getName();
      }
    }
    return StringUtils.EMPTY;
  }
}

