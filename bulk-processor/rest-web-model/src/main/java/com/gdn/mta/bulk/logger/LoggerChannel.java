package com.gdn.mta.bulk.logger;

public enum LoggerChannel {
  KAFKA("x-bulk-kafka");
  
  private String value;

  private LoggerChannel(String value) {
    this.value = value;
  }

  public String getValue() {
    return value;
  }
}
