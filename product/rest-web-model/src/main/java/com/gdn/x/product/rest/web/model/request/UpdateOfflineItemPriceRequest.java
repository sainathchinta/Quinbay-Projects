package com.gdn.x.product.rest.web.model.request;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.GdnObjects;
import com.gdn.common.web.base.BaseRequest;


@JsonIgnoreProperties(ignoreUnknown = true)
public class UpdateOfflineItemPriceRequest extends BaseRequest {

  private static final long serialVersionUID = -222437155858501824L;

  private String itemSku;
  private Double listPrice;
  private Double offerPrice;

  public UpdateOfflineItemPriceRequest() {}

  public UpdateOfflineItemPriceRequest(String itemSku, Double offerPrice) {
    this.itemSku = itemSku;
    this.offerPrice = offerPrice;
  }

  public UpdateOfflineItemPriceRequest(String itemSku, Double listPrice, Double offerPrice) {
    this.itemSku = itemSku;
    this.listPrice = listPrice;
    this.offerPrice = offerPrice;
  }

  public String getItemSku() {
    return itemSku;
  }

  public void setItemSku(String itemSku) {
    this.itemSku = itemSku;
  }

  public Double getListPrice() {
    return listPrice;
  }

  public void setListPrice(Double listPrice) {
    this.listPrice = listPrice;
  }

  public Double getOfferPrice() {
    return offerPrice;
  }

  public void setOfferPrice(Double offerPrice) {
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

  @Override
  public String toString() {
    return "UpdateOfflineItemPriceRequest{" + "itemSku='" + itemSku + '\'' + ", listPrice="
        + listPrice + ", offerPrice=" + offerPrice + '}';
  }
}
