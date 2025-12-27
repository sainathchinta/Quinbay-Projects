package com.gdn.mta.product.entity;

public enum WorkflowStates {
  DRAFT("DRAFT"),
  IN_VENDOR("IN_VENDOR"),
  QC_PASS("QC_PASS"),
  CONTENT_APPROVAL("CONTENT_APPROVAL"),
  IMAGE_APPROVAL("IMAGE_APPROVAL"),
  CONTENT_APPROVED("CONTENT_APPROVED"),
  PROCESS_IMAGE("PROCESS_IMAGE"),
  IMAGE_APPROVED("IMAGE_APPROVED"),
  ACTIVE("ACTIVE"),
  DELETED("DELETED"),
  NEED_CORRECTION("NEED_CORRECTION");

  String value;
  
  WorkflowStates(String value) {
    this.value = value;
  }

  public String getValue() {
    return this.value;
  }

  public void setValue(String value) {
    this.value = value;
  }
}
