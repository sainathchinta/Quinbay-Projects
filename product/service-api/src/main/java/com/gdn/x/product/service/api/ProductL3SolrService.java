package com.gdn.x.product.service.api;

import java.util.List;

import com.gdn.x.product.model.entity.ItemPickupPoint;

public interface ProductL3SolrService {

  /**
   * Update stock status in L3 solr
   *
   * @param productSku
   * @param status
   */
  void updateStockStatusInL3Solr(String productSku, String status, String merchantCode);


  /**
   * Update promo and wholesale item skus
   *
   * @param isPromoUpdateOnly
   * @param <T> T can be either Item or ItemPickupPoint
   */
  <T> void updatePromoOrWholesaleItemSkus(List<T> items, boolean isPromoUpdateOnly);

  /**
   * Update promo and wholesale item skus
   *
   * @param itemPickupPoint
   */
  void updatePromoOrWholesaleItemSkusByItemPickupPoint(ItemPickupPoint itemPickupPoint);

  /**
   * Update promo and wholesale item skus
   * @param updatedItemPickupPoints
   */
  void updatePromoOrWholesaleItemSkusByItemPickupPoint(List<ItemPickupPoint> updatedItemPickupPoints);

}
