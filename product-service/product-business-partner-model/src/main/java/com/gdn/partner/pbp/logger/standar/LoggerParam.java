package com.gdn.partner.pbp.logger.standar;

public enum LoggerParam {
  
  GENERIC_LOGGER("generic-logger");
  
  private String param;

  LoggerParam(String param) {
    this.param = param;
  }

  public String getParam() {
    return param;
  }

}
