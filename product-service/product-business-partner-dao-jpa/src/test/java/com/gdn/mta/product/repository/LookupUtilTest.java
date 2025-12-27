package com.gdn.mta.product.repository;

import org.junit.jupiter.api.Test;

import com.gdn.mta.product.util.GdnBaseLookup;

public class LookupUtilTest {

  @SuppressWarnings("unused")
  @Test
  public void constructorTest() throws Exception {
    GdnBaseLookup gdnBaseLookup = new GdnBaseLookup();
  }

  @SuppressWarnings("unused")
  @Test
  public void finalStaticVariableTest() throws Exception {
    Integer productTypeRegular = GdnBaseLookup.PRODUCT_TYPE_REGULAR;
    Integer productTypeBigProduct = GdnBaseLookup.PRODUCT_TYPE_BIG_PRODUCT;
    Integer productTypeBopis = GdnBaseLookup.PRODUCT_TYPE_BOPIS;
    Double productTypeRegularWeightThreshold = GdnBaseLookup.PRODUCT_TYPE_REGULAR_WEIGHT_THRESHOLD;
  }

}
