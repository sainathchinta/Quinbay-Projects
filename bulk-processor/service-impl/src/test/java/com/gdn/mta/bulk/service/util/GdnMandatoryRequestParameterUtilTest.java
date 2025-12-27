package com.gdn.mta.bulk.service.util;

import org.junit.jupiter.api.Test;
import org.slf4j.MDC;

import com.gdn.partners.bulk.util.Constant;

class GdnMandatoryRequestParameterUtilTest {

  @Test
  public void getUsernameTest() {
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, Constant.REQUEST_ID);
    GdnMandatoryRequestParameterUtil.getUsername();
  }

  @Test
  public void getUsernameNullTest() {
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, null);
    GdnMandatoryRequestParameterUtil.getUsername();
  }

  @Test
  public void getAuthenticatorTest() {
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, null);
    GdnMandatoryRequestParameterUtil.getAuthenticator();
  }
}