package com.gdn.mta.product.enums;

public enum RestrictedKeywordActionType {
  AUTO_REJECT(3), AUTO_NEED_REVISION(2), MANUAL_REVIEW_DEFAULT(1),  CHANGE_CATEGORY_AND_AUTO_APPROVE(0);

  public final int restrictedKeywordAction;

  RestrictedKeywordActionType(int restrictedKeywordAction) {
    this.restrictedKeywordAction = restrictedKeywordAction;
  }

  public int getRestrictedKeywordActionType() {
    return restrictedKeywordAction;
  }

  public static String getActionFromValue(int action) {
    for (RestrictedKeywordActionType restrictedKeywordActionType : RestrictedKeywordActionType.values()) {
      if (restrictedKeywordActionType.restrictedKeywordAction == action) {
        return restrictedKeywordActionType.name();
      }
    }
    return MANUAL_REVIEW_DEFAULT.name();
  }
}
