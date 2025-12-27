package com.gdn.x.productcategorybase.util;

import org.slf4j.MDC;
import org.springframework.util.StringUtils;

public class GdnMandatoryParameterUtil {

  public static final String REQUEST_ID_KEY_PARAMETER = "requestId";
  public static final String USERNAME_KEY_PARAMETER = "username";
  public static final String STORE_ID_KEY_PARAMETER = "storeId";
  public static final String CHANNEL_ID_KEY_PARAMETER = "channelId";
  public static final String CLIENT_ID_KEY_PARAMETER = "clientId";

  public static final String CHANNEL_ID = "web";
  public static final String CLIENT_ID = "pcb";
  public static final String REQUEST_ID = "requestId";
  public static final String STORE_ID = "10001";
  public static final String USER_NAME = "system";


  public static String getChannelId() {
    if (StringUtils.isEmpty(MDC.get(CHANNEL_ID_KEY_PARAMETER))) {
      return CHANNEL_ID;
    }
    return MDC.get(CHANNEL_ID_KEY_PARAMETER);
  }

  public static String getClientId() {
    if (StringUtils.isEmpty(MDC.get(CLIENT_ID_KEY_PARAMETER))) {
      return CLIENT_ID;
    }
    return MDC.get(CLIENT_ID_KEY_PARAMETER);
  }

  public static String getRequestId() {
    if (StringUtils.isEmpty(MDC.get(REQUEST_ID_KEY_PARAMETER))) {
      return REQUEST_ID;
    }
    return MDC.get(REQUEST_ID_KEY_PARAMETER);
  }

  public static String getStoreId() {
    if (StringUtils.isEmpty(MDC.get(STORE_ID_KEY_PARAMETER))) {
      return STORE_ID;
    }
    return MDC.get(STORE_ID_KEY_PARAMETER);
  }

  public static String getUsername() {
    if (StringUtils.isEmpty(MDC.get(USERNAME_KEY_PARAMETER))) {
      return USER_NAME;
    }
    return MDC.get(USERNAME_KEY_PARAMETER);
  }
}
