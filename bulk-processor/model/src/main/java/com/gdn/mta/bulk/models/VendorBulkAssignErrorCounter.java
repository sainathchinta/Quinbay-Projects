package com.gdn.mta.bulk.models;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
public class VendorBulkAssignErrorCounter {

  private int assigneeErrorCount;
  private int systemErrorCount;

  public void incrementAssigneeErrorCount() {
    setAssigneeErrorCount(getAssigneeErrorCount() + 1);
  }

  public void incrementSystemErrorCount(int error) {
    setSystemErrorCount(getSystemErrorCount() + error);
  }

  public void incrementSystemErrorCount() {
    setSystemErrorCount(getSystemErrorCount() + 1);
  }
}
