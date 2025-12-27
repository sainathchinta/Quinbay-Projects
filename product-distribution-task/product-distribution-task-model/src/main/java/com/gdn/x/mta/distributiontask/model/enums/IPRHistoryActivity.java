package com.gdn.x.mta.distributiontask.model.enums;

import lombok.Getter;

@Getter
public enum IPRHistoryActivity {
  ADD_TO_IPR("Added to IPR"),
  ASSIGNEE_UPDATE("Assignee Updated"),
  STATUS_UPDATE("Status Updated"),
  REMOVED_FROM_IPR("Removed from IPR");

  private final String value;

  IPRHistoryActivity(final String value) {
    this.value = value;
  }

  public static boolean isValidActivity(String iprHistoryActivity) {
    for (IPRHistoryActivity historyActivity : values()) {
      if (historyActivity.getValue().equals(iprHistoryActivity)) {
        return true;
      }
    }
    return false;
  }

}
