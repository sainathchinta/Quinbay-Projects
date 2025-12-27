package com.gdn.mta.product.controller;

import org.junit.jupiter.api.Test;

import com.gdn.mta.product.web.model.ProductBusinessPartnerControllerErrorMessage;
import com.gdn.mta.product.web.model.ProductBusinessPartnerControllerPath;

public class ProductBusinessPartnerControllerUtilTest {
  
  @SuppressWarnings("unused")
  @Test
  public void utilConstructorTest() throws Exception {
    ProductBusinessPartnerControllerPath productBusinessPartnerControllerPath =
        new ProductBusinessPartnerControllerPath();
    ProductBusinessPartnerControllerErrorMessage productBusinessPartnerControllerErrorMessage =
        new ProductBusinessPartnerControllerErrorMessage();
  }

}
