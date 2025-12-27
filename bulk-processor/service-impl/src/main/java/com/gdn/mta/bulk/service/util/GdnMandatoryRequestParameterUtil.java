package com.gdn.mta.bulk.service.util;

import org.apache.commons.lang3.StringUtils;
import org.slf4j.MDC;

import com.gdn.partners.bulk.util.Constant;

public class GdnMandatoryRequestParameterUtil {
  public static final String REQUEST_ID_KEY_PARAMETER = "requestId";
  public static final String USERNAME_KEY_PARAMETER = "username";
  public static final String STORE_ID_KEY_PARAMETER = "storeId";
  public static final String CHANNEL_ID_KEY_PARAMETER = "channelId";
  public static final String CLIENT_ID_KEY_PARAMETER = "clientId";
  public static final String AUTHENTICATOR_KEY = "authenticatorId";

  public static String getAuthenticator() {
    return MDC.get(AUTHENTICATOR_KEY);
  }

  public static String getChannelId() {
    if (StringUtils.isEmpty(MDC.get(CHANNEL_ID_KEY_PARAMETER))) {
      return Constant.CHANNEL_ID;
    }
    return MDC.get(CHANNEL_ID_KEY_PARAMETER);
  }

  public static String getClientId() {
    if (StringUtils.isEmpty(MDC.get(CLIENT_ID_KEY_PARAMETER))) {
      return Constant.CLIENT_ID;
    }
    return MDC.get(CLIENT_ID_KEY_PARAMETER);
  }

  public static String getRequestId() {
    if (StringUtils.isEmpty(MDC.get(REQUEST_ID_KEY_PARAMETER))) {
      return Constant.REQUEST_ID;
    }
    return MDC.get(REQUEST_ID_KEY_PARAMETER);
  }

  public static String getStoreId() {
    if (StringUtils.isEmpty(MDC.get(STORE_ID_KEY_PARAMETER))) {
      return Constant.STORE_ID;
    }
    return MDC.get(STORE_ID_KEY_PARAMETER);
  }

  public static String getUsername() {
    if (StringUtils.isEmpty(MDC.get(USERNAME_KEY_PARAMETER))) {
      return Constant.USER_NAME;
    }
    return MDC.get(USERNAME_KEY_PARAMETER);
  }
}
