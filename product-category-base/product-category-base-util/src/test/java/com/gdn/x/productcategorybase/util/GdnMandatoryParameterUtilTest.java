package com.gdn.x.productcategorybase.util;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.MockitoAnnotations;
import org.slf4j.MDC;

public class GdnMandatoryParameterUtilTest {


  @BeforeEach
  void setUp() {
    MockitoAnnotations.openMocks(this);
  }

  @AfterEach
  void tearDown() {
    MDC.clear();
  }

  @Test
  void getChannelId() {
    Assertions.assertEquals(GdnMandatoryParameterUtil.getChannelId(),
        GdnMandatoryParameterUtil.CHANNEL_ID);
  }

  @Test
  void getClientId() {
    Assertions.assertEquals(GdnMandatoryParameterUtil.getClientId(),
        GdnMandatoryParameterUtil.CLIENT_ID);
  }

  @Test
  void getRequestId() {
    Assertions.assertEquals(GdnMandatoryParameterUtil.getRequestId(),
        GdnMandatoryParameterUtil.REQUEST_ID);
  }

  @Test
  void getStoreId() {
    Assertions.assertEquals(GdnMandatoryParameterUtil.getStoreId(),
        GdnMandatoryParameterUtil.STORE_ID);
  }

  @Test
  void getUsername() {
    Assertions.assertEquals(GdnMandatoryParameterUtil.getUsername(),
        GdnMandatoryParameterUtil.USER_NAME);
  }


  @Test
  void getChannelIdWithMDC() {
    MDC.put(GdnMandatoryParameterUtil.CHANNEL_ID_KEY_PARAMETER, "channelId2");
    Assertions.assertEquals(GdnMandatoryParameterUtil.getChannelId(), "channelId2");
  }

  @Test
  void getClientIdWithMDC() {
    MDC.put(GdnMandatoryParameterUtil.CLIENT_ID_KEY_PARAMETER, "clientId2");
    Assertions.assertEquals(GdnMandatoryParameterUtil.getClientId(), "clientId2");
  }

  @Test
  void getRequestIdWithMDC() {
    MDC.put(GdnMandatoryParameterUtil.REQUEST_ID_KEY_PARAMETER, "requestId2");
    Assertions.assertEquals(GdnMandatoryParameterUtil.getRequestId(), "requestId2");
  }

  @Test
  void getStoreIdWithMDC() {
    MDC.put(GdnMandatoryParameterUtil.STORE_ID_KEY_PARAMETER, "storeId2");
    Assertions.assertEquals(GdnMandatoryParameterUtil.getStoreId(), "storeId2");
  }

  @Test
  void getUsernameWithMDC() {
    MDC.put(GdnMandatoryParameterUtil.USERNAME_KEY_PARAMETER, "username2");
    Assertions.assertEquals(GdnMandatoryParameterUtil.getUsername(), "username2");
  }
}