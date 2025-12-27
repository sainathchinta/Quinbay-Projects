package com.gdn.x.product.rest.web.model.request;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.GdnObjects;
import com.gdn.common.web.base.BaseRequest;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.ToString;

@JsonIgnoreProperties(ignoreUnknown = true)
@NoArgsConstructor
@AllArgsConstructor
@ToString
@Data
public class UpsertOfflineItemRequest extends BaseRequest {

  private static final long serialVersionUID = 5967893529297099453L;

  private String offlineItemId;
  private String itemSku;
  private String pickupPointCode;
  private Double listPrice;
  private Double offerPrice;
  private boolean cncActive = true;
  private boolean isBuyable = false;
  private boolean isDiscoverable = false;

  public UpsertOfflineItemRequest(String offlineItemId, String itemSku, String pickupPointCode,
      Double offerPrice) {
    this.offlineItemId = offlineItemId;
    this.itemSku = itemSku;
    this.pickupPointCode = pickupPointCode;
    this.offerPrice = offerPrice;
  }

  public UpsertOfflineItemRequest(String offlineItemId, String itemSku, String pickupPointCode,
    Double listPrice, Double offerPrice) {
    this.offlineItemId = offlineItemId;
    this.itemSku = itemSku;
    this.pickupPointCode = pickupPointCode;
    this.listPrice = listPrice;
    this.offerPrice = offerPrice;
  }

  @Override
  public boolean equals(Object obj) {
    return GdnObjects.equals(this, obj);
  }

  @Override
  public int hashCode() {
    return GdnObjects.hashCode(this);
  }
}
