package com.gdn.partners.pbp.workflow;

public enum WorkflowProcessCode {
  UPDATE_REJECTED_PRODUCT("UPDATE_REJECTED_PRODUCT", "Update Reject Product"),
  CREATE("CREATE", "Create"),
  APPROVE_DRAFT("APPROVE_DRAFT", "Approve Draft"),
  APPROVE_QC("APPROVE_QC", "Approve QC"),
  CREATE_APPROVAL("CREATE_APPROVAL", "Create Approval"),
  APPROVE_CONTENT("APPROVE_CONTENT", "Approve Content"),
  PROCESS_IMAGE("PROCESS_IMAGE", "Process Image"),
  APPROVE_IMAGE("APPROVE_IMAGE", "Approve Image"),
  ACTIVATE("ACTIVATE", "Activate"),
  REJECT_IMAGE("REJECT_IMAGE", "Reject Image"),
  CREATE_DIRECT("CREATE_DIRECT", "Create Direct"), 
  DELETE("DELETE", "Delete"),
  RETURN_FOR_CORRECTION("RETURN_FOR_CORRECTION", "Need for Correction"),
  RESUBMIT("RESUBMIT", "Resubmit"),
  CREATE_PRODUCT("CREATE_PRODUCT", "CreateProduct"),
  CREATE_PRODUCT_DIRECT("CREATE_PRODUCT_DIRECT", "if product created from flow 3");


  private String value;
  private String desc;

  WorkflowProcessCode(String value, String desc) {
    this.value = value;
    this.desc = desc;
  }

  public String getValue() {
    return value;
  }

  public void setValue(String value) {
    this.value = value;
  }

  public String getDesc() {
    return desc;
  }

  public void setDesc(String desc) {
    this.desc = desc;
  }
}
