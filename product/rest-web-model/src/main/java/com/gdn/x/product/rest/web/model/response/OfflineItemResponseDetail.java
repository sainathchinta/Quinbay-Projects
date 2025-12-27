package com.gdn.x.product.rest.web.model.response;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

import java.io.Serializable;

@JsonIgnoreProperties(ignoreUnknown = true)
public class OfflineItemResponseDetail implements Serializable {

  private static final long serialVersionUID = -8719046195918772816L;

  private String itemSku;
  private String merchantSku;
  private String errorCode;

  public String getItemSku() {
    return itemSku;
  }

  public void setItemSku(String itemSku) {
    this.itemSku = itemSku;
  }

  public String getMerchantSku() {
    return merchantSku;
  }

  public void setMerchantSku(String merchantSku) {
    this.merchantSku = merchantSku;
  }

  public String getErrorCode() {
    return errorCode;
  }

  public void setErrorCode(String errorCode) {
    this.errorCode = errorCode;
  }

  @Override
  public String toString() {
    final StringBuilder sb = new StringBuilder("OfflineItemResponseDetail{");
    sb.append("itemSku='").append(itemSku).append('\'');
    sb.append(", merchantSku='").append(merchantSku).append('\'');
    sb.append(", errorCode='").append(errorCode).append('\'');
    sb.append('}');
    return sb.toString();
  }
}
