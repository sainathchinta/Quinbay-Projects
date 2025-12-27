package com.gdn.partners.pcu.internal.service.impl.util;

import java.util.Map;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

/**
 * Created by govind on 13/01/2019 AD.
 */
public class ProductRevisionReasonsUtilTest {

  private static final String TYPE = "productDraftInternal";
  public static final String REVISION_VARIANT_COMBINE_ID = "PRODUCT.REVISION-REASON.VARIANT.COMBINE";

  @BeforeEach
  public void setUp() {
  }

  @Test
  public void getProductRevisionReasonsTest(){
    Map<String, String> response = ProductRevisionReasonsUtil.getProductRevisionReasons(TYPE);
    Assertions.assertEquals("Varian seharusnya digabung", response.get(REVISION_VARIANT_COMBINE_ID));
  }

  @Test
  public void getProductRevisionReasons_whenTypeNonScreeningTest(){
    Map<String, String> response = ProductRevisionReasonsUtil.getProductRevisionReasons("Non-screening");
    Assertions.assertNotEquals("Varian seharusnya digabung", response.get(REVISION_VARIANT_COMBINE_ID));
  }
}
