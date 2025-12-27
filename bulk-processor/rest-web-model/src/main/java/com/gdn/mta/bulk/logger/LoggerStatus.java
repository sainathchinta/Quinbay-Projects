package com.gdn.mta.bulk.logger;

public enum LoggerStatus {
  
  INFO("info"),
  SUCCESS("success"),
  ERROR("error");
  
  private String value;

  private LoggerStatus(String value) {
    this.value = value;
  }

  public String getValue() {
    return value;
  }
  
}

