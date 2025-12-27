package com.gdn.mta.bulk.logger;

public enum LoggerClient {
  MTAAPI("mta-api");
  
  private String value;

  private LoggerClient(String value) {
    this.value = value;
  }

  public String getValue() {
    return value;
  }
}
