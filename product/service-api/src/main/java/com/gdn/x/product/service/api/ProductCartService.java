package com.gdn.x.product.service.api;


public interface ProductCartService {
  /**
   * @param storeId must not be blank
   * @param itemSku must not be blank
   * @return true if product is eligible to buy
   * @throws Exception if fail
   */
  boolean isProductEligibleToBuy(String storeId, String itemSku) throws Exception;

}
