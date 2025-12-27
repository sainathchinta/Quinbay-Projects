package com.gdn.x.product.outbound.api;

import com.gdn.x.inventory.v2.rest.web.model.transaction.request.WebInventoryDeleteByWebItemSkuAndPickupPointCodeDTO;

import java.util.List;

public interface InventoryOutbound {

  /**
   * Delete L5's from inventory
   *
   * @param requestId
   * @param username
   * @param request
   * @return
   */
  boolean deleteByItemSkuAndPickupPointCode(String requestId, String username,
      List<WebInventoryDeleteByWebItemSkuAndPickupPointCodeDTO> request);
  /**
   * check if product is in stock or out of stock base don the switch to call new inventory api or old inventory api
   * @param productSku
   * @return
   */
  boolean isProductInStock(String productSku, boolean isPreOrder);
}
