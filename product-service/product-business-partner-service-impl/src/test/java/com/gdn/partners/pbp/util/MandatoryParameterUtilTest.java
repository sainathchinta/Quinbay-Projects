package com.gdn.partners.pbp.util;

import java.lang.reflect.Constructor;

import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.MatcherAssert.assertThat;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import com.gdn.mta.product.util.GdnMandatoryRequestParameterUtil;
import com.gdn.common.web.param.MandatoryRequestParam;

public class MandatoryParameterUtilTest {
  
  private static String STORE_ID = "storeId";
  private static String CLIENT_ID = "clientId";
  private static String CHANNEL_ID = "channelId";
  private static String REQUEST_ID = "requestId";
  private static String USERNAME = "username";

  @BeforeEach
  public void setUp() throws Exception {}

  @AfterEach
  public void tearDown() throws Exception {}
  
  @Test
  public void __constructor_test() throws Exception {
    Constructor<MandatoryParameterUtil> constructor = MandatoryParameterUtil.class.getDeclaredConstructor();
    constructor.setAccessible(true);
    MandatoryParameterUtil mandatoryParameterUtil = constructor.newInstance();
    Assertions.assertNotNull(mandatoryParameterUtil);
  }

  @Test
  public void testMandatoryParameterSetter() throws Exception {
    MandatoryRequestParam mandatoryRequestParam =  MandatoryRequestParam
        .generateMandatoryRequestParam(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USERNAME, null);
    MandatoryParameterUtil.mandatoryParameterSetter(mandatoryRequestParam);
    assertThat(GdnMandatoryRequestParameterUtil.getStoreId(), equalTo(STORE_ID));
    assertThat(GdnMandatoryRequestParameterUtil.getClientId(), equalTo(CLIENT_ID));
    assertThat(GdnMandatoryRequestParameterUtil.getChannelId(), equalTo(CHANNEL_ID));
    assertThat(GdnMandatoryRequestParameterUtil.getRequestId(), equalTo(REQUEST_ID));
    assertThat(GdnMandatoryRequestParameterUtil.getUsername(), equalTo(USERNAME));
  }

}
