package com.gdn.partners.pcu.external.web.model.request;

public enum ProcessFileType {
  EVIDENCE_FILE("EVIDENCE_FILE");

  private final String value;

  private ProcessFileType(String value) {
    this.value = value;
  }

  public String getValue() {
    return value;
  }
}
