package com.gdn.partner.pbp.logger.standar;

public class LoggerAttributeModel {
  
  private Object className;
  private String methodName;
  private String bpCode;
  private String username;
  private String requestId;
  private String storeId;
  private String channelId;
  private String clientId;
  private LoggerAspect loggerAspect;
  private String detail;
  private String request;
  
  public LoggerAttributeModel(Object className, String methodName, String bpCode, String username,
      String requestId, String storeId, String channelId, String clientId,
      LoggerAspect loggerAspect, String detail, String request) {
    super();
    this.className = className;
    this.methodName = methodName;
    this.bpCode = bpCode;
    this.username = username;
    this.requestId = requestId;
    this.storeId = storeId;
    this.channelId = channelId;
    this.clientId = clientId;
    this.loggerAspect = loggerAspect;
    this.detail = detail;
    this.request = request;
  }
  
  public String getRequest() {
    return request;
  }

  public void setRequest(String request) {
    this.request = request;
  }

  public Object getClassName() {
    return className;
  }

  public void setClassName(Object className) {
    this.className = className;
  }

  public String getMethodName() {
    return methodName;
  }

  public void setMethodName(String methodName) {
    this.methodName = methodName;
  }

  public String getBpCode() {
    return bpCode;
  }

  public void setBpCode(String bpCode) {
    this.bpCode = bpCode;
  }

  public String getUsername() {
    return username;
  }

  public void setUsername(String username) {
    this.username = username;
  }

  public String getRequestId() {
    return requestId;
  }

  public void setRequestId(String requestId) {
    this.requestId = requestId;
  }

  public String getStoreId() {
    return storeId;
  }

  public void setStoreId(String storeId) {
    this.storeId = storeId;
  }

  public String getChannelId() {
    return channelId;
  }

  public void setChannelId(String channelId) {
    this.channelId = channelId;
  }

  public String getClientId() {
    return clientId;
  }

  public void setClientId(String clientId) {
    this.clientId = clientId;
  }

  public LoggerAspect getLoggerAspect() {
    return loggerAspect;
  }

  public void setLoggerAspect(LoggerAspect loggerAspect) {
    this.loggerAspect = loggerAspect;
  }

  public String getDetail() {
    return detail;
  }

  public void setDetail(String detail) {
    this.detail = detail;
  }
}
