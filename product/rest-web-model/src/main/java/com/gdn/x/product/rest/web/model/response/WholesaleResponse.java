package com.gdn.x.product.rest.web.model.response;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.GdnObjects;
import com.gdn.common.web.base.BaseResponse;
import com.gdn.x.product.rest.web.model.WholesaleRule;
import java.util.ArrayList;
import java.util.List;

@JsonIgnoreProperties(ignoreUnknown = true)
public class WholesaleResponse extends BaseResponse {

  private static final long serialVersionUID = 1L;

  private String itemSku;
  private String itemCode;
  private double listPrice;
  private double offerPrice;
  private double productAdjustmentPrice;
  private List<WholesaleRule> wholesaleRules = new ArrayList<WholesaleRule>();
  private int total;

  public WholesaleResponse() {
  }

  public WholesaleResponse(String itemSku, String itemCode, double listPrice, double offerPrice,
      double productAdjustmentPrice, List<WholesaleRule> wholesaleRules, int total) {
    this.itemSku = itemSku;
    this.itemCode = itemCode;
    this.listPrice = listPrice;
    this.offerPrice = offerPrice;
    this.productAdjustmentPrice = productAdjustmentPrice;
    this.wholesaleRules = wholesaleRules;
    this.total = total;
  }

  @Override
  public boolean equals(Object obj) {
    return GdnObjects.equals(this, obj);
  }

  public String getItemCode() {
    return itemCode;
  }

  public String getItemSku() {
    return itemSku;
  }

  public double getListPrice() {
    return listPrice;
  }

  public double getOfferPrice() {
    return offerPrice;
  }

  public double getProductAdjustmentPrice() {
    return productAdjustmentPrice;
  }

  public int getTotal() {
    return total;
  }

  @Override
  public int hashCode() {
    return GdnObjects.hashCode(this);
  }

  public void setItemCode(String itemCode) {
    this.itemCode = itemCode;
  }

  public void setItemSku(String itemSku) {
    this.itemSku = itemSku;
  }

  public void setListPrice(double listPrice) {
    this.listPrice = listPrice;
  }

  public void setOfferPrice(double offerPrice) {
    this.offerPrice = offerPrice;
  }

  public void setProductAdjustmentPrice(double productAdjustmentPrice) {
    this.productAdjustmentPrice = productAdjustmentPrice;
  }

  public void setTotal(int total) {
    this.total = total;
  }

  public List<WholesaleRule> getWholesaleRules() {
    return wholesaleRules;
  }

  public void setWholesaleRules(List<WholesaleRule> wholesaleRules) {
    this.wholesaleRules = wholesaleRules;
  }

  @Override
  public String toString() {
    return String.format(
        "WholesaleResponse [itemSku=%s, itemCode=%s, listPrice=%s, offerPrice=%s, " +
            "productAdjustmentPrice=%s, wholesaleRules=%s, total=%s]",
        this.itemSku, this.itemCode, this.listPrice, this.offerPrice, this.productAdjustmentPrice,
        this.wholesaleRules, this.total);
  }
}
