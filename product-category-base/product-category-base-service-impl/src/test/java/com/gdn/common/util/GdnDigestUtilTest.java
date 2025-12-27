package com.gdn.common.util;

import java.lang.reflect.Constructor;

import org.junit.jupiter.api.Test;


public class GdnDigestUtilTest {

  @Test
  public void testInitialize() throws Exception {
    Constructor<GdnDigestUtil> constructor = GdnDigestUtil.class.getDeclaredConstructor();
    constructor.setAccessible(true);
    constructor.newInstance();
    constructor.setAccessible(false);
  }

}
