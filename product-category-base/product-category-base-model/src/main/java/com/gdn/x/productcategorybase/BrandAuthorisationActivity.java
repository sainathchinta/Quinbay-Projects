package com.gdn.x.productcategorybase;

public enum BrandAuthorisationActivity {
  CREATE("create", "Brand Auth created"),
  STATUS_UPDATE("Status Update",
    "Brand auth status updated"),
  DOCUMENT_LINK_UPDATE("New Brand Auth document added",
    "New Brand Auth document added"),
  START_DATE_UPDATE("Start Date Update", "Brand Auth Start Date Updated"),

  END_DATE_UPDATE("End Date Update", "Brand Auth End Date Updated"),

  DELETE("Brand Auth Deleted", "Brand Auth deleted"), REJECTED("Brand auth Rejected",
    "Brand auth Rejected"), NEED_REVISION("Brand auth sent to need revision",
    "Brand auth sent to need revision"), REQUESTED("Brand Auth Requested",
    "Brand Auth Requested"), APPROVED("Brand Auth Approved", "Brand Auth Approved");
  private final String activity;
  private final String description;

  BrandAuthorisationActivity(String activity, String description) {
    this.activity = activity;
    this.description = description;
  }

  public String getDescription() {
    return description;
  }
}
