package com.gdn.partners.pcu.internal.service.model;

import java.util.Arrays;

import org.apache.commons.lang3.StringUtils;

public enum BulkInternalPath {
  BULK_INTERNAL_PROCESS_PATH("internal/"),
  BULK_INTERNAL_SUSPENSION_PATH("suspension/"),
  INTERNAL_STORE_COPY_PATH("store-copy/"),
  INTERNAL_SALES_CATEGORY_UPDATE_PATH("sales-category/"),
  INTERNAL_RECAT_PATH("recat/"),
  INTERNAL_VENDOR_PATH("vendor/"),
  INTERNAL_BULK_CONFIGURATION_PATH("config/");

  private String value;

  private BulkInternalPath(String value) {
    this.value = value;
  }

  public String getValue() {
    return value;
  }
  public static BulkInternalProcessType getBulkInternalPath(String process) {
    return Arrays.stream(BulkInternalProcessType.values())
        .filter(element -> StringUtils.equalsIgnoreCase(element.getValue(), process)).findAny().orElse(null);
  }
}
