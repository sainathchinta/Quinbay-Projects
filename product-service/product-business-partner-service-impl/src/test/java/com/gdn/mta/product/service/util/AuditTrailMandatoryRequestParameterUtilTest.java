package com.gdn.mta.product.service.util;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.MockitoAnnotations;
import org.slf4j.MDC;

/**
 * Created by alok on 12/04/17.
 */
public class AuditTrailMandatoryRequestParameterUtilTest {

  private static final String CLIENT_HOST = "10.111.11.11";


  @BeforeEach
  public void setUp() throws Exception {

    MockitoAnnotations.initMocks(this);
  }

  @Test
  public void getClientHostTestOk() {
    MDC.put("clientHost", CLIENT_HOST);
    String clintHost = AuditTrailMandatoryRequestParameterUtil.getClientHost();
    Assertions.assertEquals(CLIENT_HOST, clintHost);
  }

  @Test
  public void testMDC() {
    String key = "clientHost";
    String value = "10.111.11.11";

    MDC.put(key, value);
    String result = MDC.get(key);

    Assertions.assertEquals(value, result);

    MDC.remove(key);
  }

  @AfterEach
  public void tearDown() throws Exception {

  }

}
