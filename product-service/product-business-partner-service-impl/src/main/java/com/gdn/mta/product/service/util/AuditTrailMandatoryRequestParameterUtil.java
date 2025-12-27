package com.gdn.mta.product.service.util;

import org.slf4j.MDC;

public class AuditTrailMandatoryRequestParameterUtil {
  public static final String CLIENT_HOST_KEY = "clientHost";

  private AuditTrailMandatoryRequestParameterUtil() {}

  public static String getClientHost() {
    return MDC.get(CLIENT_HOST_KEY);
  }
}
