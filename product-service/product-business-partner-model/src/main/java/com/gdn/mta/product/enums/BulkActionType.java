package com.gdn.mta.product.enums;

import java.util.HashMap;
import java.util.Map;

public enum BulkActionType {
  ASSIGN("assign"),
  UN_ASSIGN("unAssign"),
  SEND_FOR_REVISION("sendForRevision"),
  REJECT("reject");

  private static final Map<String, BulkActionType> lookUpMap = new HashMap<>();

  static {
    for (BulkActionType bulkActionType: BulkActionType.values()) {
      lookUpMap.put(bulkActionType.getBulkActionName(), bulkActionType);
    }
  }

  private final String bulkActionName;

  BulkActionType(String bulkActionName) {
    this.bulkActionName = bulkActionName;
  }

  public String getBulkActionName() {
    return bulkActionName;
  }

  public static BulkActionType getBulkActionTypeByBulkActionName(String bulkActionName) {
    return lookUpMap.get(bulkActionName);
  }
}
