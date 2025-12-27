package com.gdn.mta.bulk.util;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.slf4j.MDC;

import static org.junit.jupiter.api.Assertions.assertEquals;

import com.gdn.mta.bulk.service.util.GdnMandatoryRequestParameterUtil;
import com.gdn.partners.bulk.util.Constant;

public class GdnMandatoryRequestParameterUtilTest {

  private static final String REQUEST_ID_KEY_PARAMETER = "requestId";
  private static final String USERNAME_KEY_PARAMETER = "username";
  private static final String STORE_ID_KEY_PARAMETER = "storeId";
  private static final String CHANNEL_ID_KEY_PARAMETER = "channelId";
  private static final String CLIENT_ID_KEY_PARAMETER = "clientId";
  private static final String AUTHENTICATOR_KEY = "authenticatorId";

  @BeforeEach
  public void setUp() {
    MDC.clear();
  }

  @Test
  public void testGetAuthenticator() {
    MDC.put(GdnMandatoryRequestParameterUtil.AUTHENTICATOR_KEY, AUTHENTICATOR_KEY);
    Assertions.assertEquals(AUTHENTICATOR_KEY, GdnMandatoryRequestParameterUtil.getAuthenticator());
  }

  @Test
  public void testGetChannelId() {
    Assertions.assertEquals(Constant.CHANNEL_ID, GdnMandatoryRequestParameterUtil.getChannelId());

    MDC.put(GdnMandatoryRequestParameterUtil.CHANNEL_ID_KEY_PARAMETER, CHANNEL_ID_KEY_PARAMETER);
    Assertions.assertEquals(CHANNEL_ID_KEY_PARAMETER, GdnMandatoryRequestParameterUtil.getChannelId());
  }

  @Test
  public void testGetClientId() {
    Assertions.assertEquals(Constant.CLIENT_ID, GdnMandatoryRequestParameterUtil.getClientId());

    MDC.put(GdnMandatoryRequestParameterUtil.CLIENT_ID_KEY_PARAMETER, CLIENT_ID_KEY_PARAMETER);
    Assertions.assertEquals(CLIENT_ID_KEY_PARAMETER, GdnMandatoryRequestParameterUtil.getClientId());
  }

  @Test
  public void testGetRequestId() {
    Assertions.assertEquals(Constant.REQUEST_ID, GdnMandatoryRequestParameterUtil.getRequestId());

    MDC.put(GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER, REQUEST_ID_KEY_PARAMETER);
    Assertions.assertEquals(REQUEST_ID_KEY_PARAMETER, GdnMandatoryRequestParameterUtil.getRequestId());
  }

  @Test
  public void testGetStoreId() {
    Assertions.assertEquals(Constant.STORE_ID, GdnMandatoryRequestParameterUtil.getStoreId());

    MDC.put(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER, STORE_ID_KEY_PARAMETER);
    Assertions.assertEquals(STORE_ID_KEY_PARAMETER, GdnMandatoryRequestParameterUtil.getStoreId());
  }

  @Test
  public void testGetUsername() {
    Assertions.assertEquals(Constant.USER_NAME, GdnMandatoryRequestParameterUtil.getUsername());

    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, USERNAME_KEY_PARAMETER);
    Assertions.assertEquals(USERNAME_KEY_PARAMETER, GdnMandatoryRequestParameterUtil.getUsername());
  }
}
