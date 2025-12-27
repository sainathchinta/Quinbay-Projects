package com.gdn.mta.bulk.util;

public class LoggerTemplateUtil {

  private static String FEATURE = "feature=";
  private static String REQUEST_ID = "requestId=";
  private static String USERNAME = "username=";
  private static String BP_CODE = "bpCode=";
  private static String DETAIL = "detail=";
  private static String ERROR_MSG = "errorMsg=";
  private static String DIVIDER = " | ";

  private LoggerTemplateUtil() {
  }

  public static String generateAuditMtaApiInfo(String feature, String requestId,
      String username, String bpCode, String detail) {
    StringBuilder sb = new StringBuilder();
    sb.append(FEATURE).append(feature).append(DIVIDER);
    sb.append(REQUEST_ID).append(requestId).append(DIVIDER);
    sb.append(USERNAME).append(username).append(DIVIDER);
    sb.append(BP_CODE).append(bpCode).append(DIVIDER);
    sb.append(DETAIL).append(detail);
    return sb.toString();
  }
  
  public static String generateAuditMtaApiError(String feature, String requestId, 
      String username, String bpCode, String detail, String errorMsg) {
    StringBuilder sb = new StringBuilder();
    sb.append(FEATURE).append(feature).append(DIVIDER);
    sb.append(REQUEST_ID).append(requestId).append(DIVIDER);
    sb.append(USERNAME).append(username).append(DIVIDER);
    sb.append(BP_CODE).append(bpCode).append(DIVIDER);
    sb.append(DETAIL).append(detail).append(DIVIDER);
    sb.append(ERROR_MSG).append(errorMsg);
    return sb.toString();
  }

}
