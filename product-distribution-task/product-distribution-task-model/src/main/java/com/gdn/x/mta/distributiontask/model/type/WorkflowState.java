package com.gdn.x.mta.distributiontask.model.type;

public enum WorkflowState {
  UNASSIGNED("Unassigned"),
  IN_REVIEW("In Review"),
  NEED_CORRECTION("Need Correction"),
  REJECTED("Rejected"),
  QC_REJECTED("QC Rejected"),
  PASSED("Passed"),
  CONTENT_NEED_CORRECTION("Content Need Correction"),
  IMAGE_NEED_CORRECTION("Image Need Correction"),
  CONTENT_AND_IMAGE_NEED_CORRECTION("Content And Image Need Correction"),
  EXCEEDED_SLA("Exceeded SLA"),
  DELETED("Deleted");

  private String desc;

  WorkflowState(String desc) {
    this.desc = desc;
  }

  public String getDesc() {
    return this.desc;
  }

  public void setDesc(String desc) {
    this.desc = desc;
  }


}

