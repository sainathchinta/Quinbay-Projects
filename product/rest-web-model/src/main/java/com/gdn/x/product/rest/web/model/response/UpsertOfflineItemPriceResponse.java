package com.gdn.x.product.rest.web.model.response;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.GdnObjects;
import com.gdn.common.web.base.BaseResponse;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.ToString;
/**
 * Created by i.wiranatha on 2/7/2018.
 */

@JsonIgnoreProperties(ignoreUnknown = true)
@NoArgsConstructor
@AllArgsConstructor
@ToString
@Data
public class UpsertOfflineItemPriceResponse extends BaseResponse {

  private static final long serialVersionUID = 1L;

  private String offlineItemId;
  private String itemSku;
  private String merchantSku;
  private String pickupPointCode;
  private String externalPickupPointCode;
  private Double listPrice;
  private Double offerPrice;
  private boolean success;
  private String errorMessage;
  private boolean cncActive;
  private boolean isBuyable;
  private boolean isDiscoverable;
  private boolean isNew;
  private String itemName;
  private String productSku;
  private String itemCode;
  private boolean fbbActivated;

  @Override
  public boolean equals(Object obj) {
    return GdnObjects.equals(this, obj);
  }

  @Override
  public int hashCode() {
    return GdnObjects.hashCode(this);
  }

  public UpsertOfflineItemPriceResponse(String offlineItemId, String itemSku, String merchantSku,
      String pickupPointCode, String externalPickupPointCode, Double listPrice, Double offerPrice, boolean success,
      String errorMessage, boolean cncActive, boolean isBuyable, boolean isDiscoverable, boolean isNew, String itemName,
      String productSku) {
    this.offlineItemId = offlineItemId;
    this.itemSku = itemSku;
    this.merchantSku = merchantSku;
    this.pickupPointCode = pickupPointCode;
    this.externalPickupPointCode = externalPickupPointCode;
    this.listPrice = listPrice;
    this.offerPrice = offerPrice;
    this.success = success;
    this.errorMessage = errorMessage;
    this.cncActive = cncActive;
    this.isBuyable = isBuyable;
    this.isDiscoverable = isDiscoverable;
    this.isNew = isNew;
    this.itemName = itemName;
    this.productSku = productSku;
  }
}
