package com.gdn.x.mta.distributiontask.inbound.util;

import org.slf4j.MDC;

import com.gdn.x.mta.distributiontask.util.GdnMandatoryRequestParameterUtil;
import com.gdn.common.web.param.MandatoryRequestParam;

public final class MandatoryParameterUtil {
  private MandatoryParameterUtil() {
  }

  public static void mandatoryParameterSetter(MandatoryRequestParam mandatoryRequestParam) {
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, mandatoryRequestParam.getUsername());
    MDC.put(GdnMandatoryRequestParameterUtil.CLIENT_ID_KEY_PARAMETER, mandatoryRequestParam.getClientId());
    MDC.put(GdnMandatoryRequestParameterUtil.CHANNEL_ID_KEY_PARAMETER, mandatoryRequestParam.getChannelId());
    MDC.put(GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER, mandatoryRequestParam.getRequestId());
    MDC.put(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER, mandatoryRequestParam.getStoreId());
    MDC.put(GdnMandatoryRequestParameterUtil.AUTHENTICATOR_KEY, mandatoryRequestParam.getAuthenticator());
  }
}

