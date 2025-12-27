package com.gdn.partners.pbp.util;

import java.lang.reflect.Constructor;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;

import org.jboss.logging.MDC;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import com.gdn.mta.product.util.GdnMandatoryRequestParameterUtil;
import com.gdn.common.web.param.MandatoryRequestParam;
import com.gdn.x.inventory.v2.rest.web.model.enums.ActionKeyEnum;

public class ProductLevel3InventoryUtilTest {

  private static final String STORE_ID = "10001";
  private static final String CHANNEL_ID = "MTA";
  private static final String CLIENT_ID = "ABC";
  private static final String REQUEST_ID = "test";
  private static final String USERNAME = "test";

  @Test
  public void privateConstructor_Test() throws Exception {
    Assertions.assertTrue(Modifier.isFinal(ProductLevel3InventoryUtil.class.getModifiers()), "class must be final");
    final Constructor<ProductLevel3InventoryUtil> constructor =
        ProductLevel3InventoryUtil.class.getDeclaredConstructor();
    if (constructor.isAccessible() || !Modifier.isPrivate(constructor.getModifiers())) {
      Assertions.fail("constructor is not private");
    }
    constructor.setAccessible(true);
    constructor.newInstance();
    constructor.setAccessible(false);
    for (final Method method : ProductLevel3InventoryUtil.class.getMethods()) {
      if (!Modifier.isStatic(method.getModifiers())
          && method.getDeclaringClass().equals(ProductLevel3InventoryUtil.class)) {
        Assertions.fail("there exists a non-static method:" + method);
      }
    }
  }

  @Test
  public void generateInventoryBaseRequest_Test() {
    ProductLevel3InventoryUtil.generateInventoryBaseRequest(ActionKeyEnum.CONFIRM_PAYMENT_STOCK);
  }

  @Test
  public void generateMandatoryRequestParam_Test() throws Exception {
    MDC.put(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER, STORE_ID);
    MDC.put(GdnMandatoryRequestParameterUtil.CHANNEL_ID_KEY_PARAMETER, CHANNEL_ID);
    MDC.put(GdnMandatoryRequestParameterUtil.CLIENT_ID_KEY_PARAMETER, CLIENT_ID);
    MDC.put(GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER, REQUEST_ID);
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, USERNAME);
    ProductLevel3InventoryUtil.generateMandatoryRequestParam();
  }

}
