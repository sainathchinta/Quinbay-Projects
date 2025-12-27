package com.gdn.partners.pcu.internal.web.model.enums;

public enum AutoApprovedActionsEnum {
  ACCEPT, SUSPEND, AUTO_HEAL;

  public static boolean validAction(String name) {
    for (AutoApprovedActionsEnum actionTypeEnum : AutoApprovedActionsEnum.values()) {
      if (actionTypeEnum.name().equals(name)) {
        return true;
      }
    }
    return false;
  }
}
