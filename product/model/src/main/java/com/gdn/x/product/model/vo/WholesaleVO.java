package com.gdn.x.product.model.vo;

import com.gdn.common.base.GdnObjects;
import org.apache.commons.lang3.builder.ToStringBuilder;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

public class WholesaleVO implements Serializable {

  private static final long serialVersionUID = 1L;

  private String itemSku;
  private String itemCode;
  private double listPrice;
  private double offerPrice;
  private double productAdjustmentPrice;
  private List<WholesaleRuleVO> wholesaleRules = new ArrayList<>();
  private int total;

  public WholesaleVO() {
  }

  public WholesaleVO(String itemSku, String itemCode, double listPrice, double offerPrice,
      double productAdjustmentPrice, List<WholesaleRuleVO> wholesaleRules,
      int total) {
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

  public List<WholesaleRuleVO> getWholesaleRules() {
    return wholesaleRules;
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

  public void setWholesaleRules(List<WholesaleRuleVO> wholesaleRules) {
    this.wholesaleRules = wholesaleRules;
  }

  @Override
  public String toString() {
    return new ToStringBuilder(this).append("itemSku", itemSku).append("itemCode", itemCode)
        .append("listPrice", listPrice).append("offerPrice", offerPrice)
        .append("productAdjustmentPrice", productAdjustmentPrice)
        .append("wholesaleRules", wholesaleRules).append("total", total)
        .toString();
  }
}
