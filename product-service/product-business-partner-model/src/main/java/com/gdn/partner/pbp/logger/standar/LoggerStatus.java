package com.gdn.partner.pbp.logger.standar;

public enum LoggerStatus {
  
  INFO("info"),
  SUCCESS("success"),
  ERROR("error");
  
  private String value;

  LoggerStatus(String value) {
    this.value = value;
  }

  public String getValue() {
    return value;
  }
  
}
