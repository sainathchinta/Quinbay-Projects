package com.gdn.x.product.model.vo;

import java.io.Serializable;

import com.gdn.common.base.GdnObjects;

public class ProductForTransactionVO implements Serializable {
  /**
   *
   */
  private static final long serialVersionUID = 1L;
  private String itemSku;
  private String itemCode;
  private ProductItemDetailVO itemDetail;
  private boolean cncActivated;

  public ProductForTransactionVO() {

  }

  public ProductForTransactionVO(String itemSku, String itemCode, ProductItemDetailVO itemDetail, boolean cncActivated) {
    this.itemSku = itemSku;
    this.itemCode = itemCode;
    this.itemDetail = itemDetail;
    this.cncActivated = cncActivated;
  }

  @Override
  public boolean equals(Object obj) {
    return GdnObjects.equals(this, obj);
  }

  public String getItemCode() {
    return itemCode;
  }


  public ProductItemDetailVO getItemDetail() {
    return this.itemDetail;
  }

  public String getItemSku() {
    return this.itemSku;
  }

  public boolean isCncActivated() {
    return cncActivated;
  }

  @Override
  public int hashCode() {
    return GdnObjects.hashCode(this);
  }

  public void setItemCode(String itemCode) {
    this.itemCode = itemCode;
  }

  public void setItemDetail(ProductItemDetailVO itemDetail) {
    this.itemDetail = itemDetail;
  }

  public void setItemSku(String itemSku) {
    this.itemSku = itemSku;
  }

  public void setCncActivated(boolean cncActivated) {
    this.cncActivated = cncActivated;
  }

  @Override
  public String toString() {
    return String.format(
        "ProductForTransactionVO [itemSku=%s, itemCode=%s, itemDetail=%s, cncActivated=%s]",
        this.itemSku, this.itemCode, this.itemDetail, this.cncActivated);
  }
}
