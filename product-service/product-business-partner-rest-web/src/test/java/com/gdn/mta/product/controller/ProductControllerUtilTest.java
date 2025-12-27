package com.gdn.mta.product.controller;

import org.junit.jupiter.api.Test;

import com.gdn.mta.product.web.model.ProductControllerErrorMessage;
import com.gdn.mta.product.web.model.ProductControllerPath;

public class ProductControllerUtilTest {

  @SuppressWarnings("unused")
  @Test
  public void utilConstructorTest() throws Exception {
    ProductControllerPath productControllerPath = new ProductControllerPath();
    ProductControllerErrorMessage productControllerErrorMessage =
        new ProductControllerErrorMessage();
  }

}
