package com.gdn.x.product.rest.web.model.response;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.GdnObjects;
import com.gdn.common.web.base.BaseResponse;
import com.gdn.x.product.rest.web.model.dto.SimpleProductItemDTO;

/**
 * @author felix.w.wijaya
 *
 */
@JsonIgnoreProperties(ignoreUnknown = true)
public class SimpleProductResponse extends BaseResponse {

  private static final long serialVersionUID = 6696332810582106219L;
  private String itemSku;
  private String itemCode;
  private String description;
  private boolean buyable;
  private double offerPrice;
  private SimpleProductItemDTO itemDetail;

  public SimpleProductResponse() {}

  @Override
  public boolean equals(Object obj) {
    return GdnObjects.equals(this, obj);
  }

  public String getDescription() {
    return description;
  }

  public String getItemCode() {
    return itemCode;
  }

  public SimpleProductItemDTO getItemDetail() {
    return this.itemDetail;
  }

  public String getItemSku() {
    return this.itemSku;
  }

  public double getOfferPrice() {
    return offerPrice;
  }

  @Override
  public int hashCode() {
    return GdnObjects.hashCode(this);
  }

  public boolean isBuyable() {
    return buyable;
  }

  public void setBuyable(boolean buyable) {
    this.buyable = buyable;
  }

  public void setDescription(String description) {
    this.description = description;
  }

  public void setItemCode(String itemCode) {
    this.itemCode = itemCode;
  }

  public void setItemDetail(SimpleProductItemDTO itemDetail) {
    this.itemDetail = itemDetail;
  }

  public void setItemSku(String itemSku) {
    this.itemSku = itemSku;
  }

  public void setOfferPrice(double offerPrice) {
    this.offerPrice = offerPrice;
  }

  @Override
  public String toString() {
    return String.format(
        "SimpleProductResponse [itemSku=%s, itemCode=%s, description=%s, itemDetail=%s, toString()=%s]",
        itemSku, itemCode, description, itemDetail, super.toString());
  }
}
