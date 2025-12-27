package com.gdn.x.product.rest.web.model.response;


import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.GdnObjects;
import com.gdn.common.web.base.BaseResponse;
import com.gdn.x.product.rest.web.model.dto.ProductItemDetailDTO;

@JsonIgnoreProperties(ignoreUnknown = true)
public class ProductForTransactionResponse extends BaseResponse {

  private static final long serialVersionUID = 6696332810582106219L;
  private String itemSku;
  private String itemCode;
  private ProductItemDetailDTO itemDetail;
  private boolean cncActivated;

  public ProductForTransactionResponse() {}

  @Override
  public boolean equals(Object obj) {
    return GdnObjects.equals(this, obj);
  }

  public String getItemCode() {
    return itemCode;
  }

  public ProductItemDetailDTO getItemDetail() {
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

  public void setItemDetail(ProductItemDetailDTO itemDetail) {
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
        "ProductForTransactionResponse [itemSku=%s, itemCode=%s, itemDetail=%s, cncActivated=%s]",
        this.itemSku, this.itemCode, this.itemDetail, this.cncActivated);
  }
}
