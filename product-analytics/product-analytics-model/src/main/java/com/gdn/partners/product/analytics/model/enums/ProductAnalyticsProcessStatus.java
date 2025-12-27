package com.gdn.partners.product.analytics.model.enums;

public enum ProductAnalyticsProcessStatus {

  CREATED("created"),
  IN_PROGRESS("inProgress"),
  DONE("done"),
  FAILED("failed");

  private String status;

  ProductAnalyticsProcessStatus(String status){
    this.status = status;
  }

  public String getStatus(){
    return status;
  }

}
