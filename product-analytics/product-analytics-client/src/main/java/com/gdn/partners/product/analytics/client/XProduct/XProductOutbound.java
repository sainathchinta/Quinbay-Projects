package com.gdn.partners.product.analytics.client.XProduct;

public interface XProductOutbound {

  /**
   * check if product is shared
   *
   * @param productCode
   * @param sellerCode
   * @return
   */
  boolean isSharedProduct(String sellerCode, String productCode);
}
