package com.gdn.mta.bulk.models;

public class BulkProcessReport {

  private int totalCount;
  private int successCount;
  private int failedCount;

  public BulkProcessReport() {

  }

  public int getTotalCount() {
    return totalCount;
  }

  public void setTotalCount(int totalCount) {
    this.totalCount = totalCount;
  }

  public int getSuccessCount() {
    return successCount;
  }

  public void setSuccessCount(int successCount) {
    this.successCount = successCount;
  }

  public int getFailedCount() {
    return failedCount;
  }

  public void setFailedCount(int failedCount) {
    this.failedCount = failedCount;
  }

  @Override
  public String toString() {
    final StringBuilder sb = new StringBuilder("BulkProcessReport{");
    sb.append("totalCount=").append(totalCount);
    sb.append(", successCount=").append(successCount);
    sb.append(", failedCount=").append(failedCount);
    sb.append('}');
    return sb.toString();
  }
}
