package com.gdn.partners.pcu.internal.service.model;

import java.util.Arrays;

import org.apache.commons.lang3.StringUtils;

public enum BulkInternalProcessType {
  BULK_INTERNAL_PROCESS_TYPE("internal"),
  BULK_INTERNAL_SUSPENSION_TYPE("suspension"),
  INTERNAL_STORE_COPY("store-copy"),
  INTERNAL_SALES_CATEGORY_UPDATE("sales-category"),
  INTERNAL_RECAT("recat"),
  INTERNAL_BULK_CONFIGURATION("config"),
  RESTRICTED_KEYWORD_UPSERT("RESTRICTED_KEYWORD_UPSERT"),
  RESTRICTED_KEYWORD_DELETE("RESTRICTED_KEYWORD_DELETE"),
  BRAND_AUTH_ADD("BRAND_AUTH_ADD"),
  BRAND_AUTH_DELETE("BRAND_AUTH_DELETE"),
  BULK_APPROVAL("BULK_APPROVAL"),
  BULK_REJECTION("BULK_REJECTION"),
  INTERNAL_VENDOR_BULK("vendor"),
  MASTER_SKU_BULK_ASSIGNEE("MASTER_SKU_BULK_ASSIGNEE"),
  MASTER_SKU_BULK_REVIEW("MASTER_SKU_BULK_REVIEW"),
  AUTO_APPROVED_PRODUCTS_BULK_ASSIGN("AUTO_APPROVED_PRODUCTS_BULK_ASSIGN"),
  BULK_PRICE_UPDATE("BULK_PRICE_UPDATE"),
  IPR_PORTAL_BULK_ADD_REVIEW("IPR_PORTAL_BULK_ADD_REVIEW"),
  DEFAULT("default");

  private String value;

  private BulkInternalProcessType(String value) {
    this.value = value;
  }

  public String getValue() {
    return value;
  }
  public static BulkInternalProcessType getBulkInternalProcessType(String process) {
    return Arrays.stream(BulkInternalProcessType.values())
        .filter(element -> StringUtils.equalsIgnoreCase(element.getValue(), process)).findAny().orElse(null);
  }

  public static String getValueOrEmpty(String processType) {
    for (BulkInternalProcessType bulkInternalProcessType : values()) {
      if (bulkInternalProcessType.name().equals(processType)) {
        return bulkInternalProcessType.name();
      }
    }
    return StringUtils.EMPTY;
  }
}
