package com.gdn.mta.bulk.entity;

import java.io.Serializable;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.entity.GdnBaseDomainEventModel;

@JsonIgnoreProperties(ignoreUnknown = true)
public class BulkProcessApiUpdateProductQueue extends GdnBaseDomainEventModel implements Serializable {

  private static final long serialVersionUID = -6033053586439916354L;

  private String requestId;
  private String merchantCode;
  private String username;
  private String remoteAddress;
  private String gdnSku;
  private Double stock;
  private Integer minimumStock;
  private Double price;
  private Double salePrice;
  private Boolean buyable;
  private Boolean displayable;
  private Boolean off2OnActiveFlag;

  public BulkProcessApiUpdateProductQueue() {
    super();
  }

  public BulkProcessApiUpdateProductQueue(String requestId, String merchantCode, String gdnSku,
      Double stock, Integer minimumStock, Double price, Double salePrice, Boolean autohide,
      Boolean buyable, Boolean displayable) {
    super();
    this.requestId = requestId;
    this.merchantCode = merchantCode;
    this.gdnSku = gdnSku;
    this.stock = stock;
    this.minimumStock = minimumStock;
    this.price = price;
    this.salePrice = salePrice;
    this.buyable = buyable;
    this.displayable = displayable;
  }

  @Override
  public boolean equals(Object obj) {
    if (this == obj)
      return true;
    if (obj == null)
      return false;
    if (getClass() != obj.getClass())
      return false;
    BulkProcessApiUpdateProductQueue other = (BulkProcessApiUpdateProductQueue) obj;
    if (buyable == null) {
      if (other.buyable != null)
        return false;
    } else if (!buyable.equals(other.buyable))
      return false;
    if (displayable == null) {
      if (other.displayable != null)
        return false;
    } else if (!displayable.equals(other.displayable))
      return false;
    if (gdnSku == null) {
      if (other.gdnSku != null)
        return false;
    } else if (!gdnSku.equals(other.gdnSku))
      return false;
    if (merchantCode == null) {
      if (other.merchantCode != null)
        return false;
    } else if (!merchantCode.equals(other.merchantCode))
      return false;
    if (minimumStock == null) {
      if (other.minimumStock != null)
        return false;
    } else if (!minimumStock.equals(other.minimumStock))
      return false;
    if (off2OnActiveFlag == null) {
      if (other.off2OnActiveFlag != null)
        return false;
    } else if (!off2OnActiveFlag.equals(other.off2OnActiveFlag))
      return false;
    if (price == null) {
      if (other.price != null)
        return false;
    } else if (!price.equals(other.price))
      return false;
    if (requestId == null) {
      if (other.requestId != null)
        return false;
    } else if (!requestId.equals(other.requestId))
      return false;
    if (salePrice == null) {
      if (other.salePrice != null)
        return false;
    } else if (!salePrice.equals(other.salePrice))
      return false;
    if (stock == null) {
      if (other.stock != null)
        return false;
    } else if (!stock.equals(other.stock))
      return false;
    return true;
  }

  public Boolean getBuyable() {
    return buyable;
  }

  public Boolean getDisplayable() {
    return displayable;
  }

  public String getGdnSku() {
    return gdnSku;
  }

  public String getMerchantCode() {
    return merchantCode;
  }

  public Integer getMinimumStock() {
    return minimumStock;
  }

  public Double getPrice() {
    return price;
  }


  public String getRequestId() {
    return requestId;
  }


  public Double getSalePrice() {
    return salePrice;
  }


  public Double getStock() {
    return stock;
  }


  @Override
  public int hashCode() {
    final int prime = 31;
    int result = 1;
    result = prime * result + ((buyable == null) ? 0 : buyable.hashCode());
    result = prime * result + ((displayable == null) ? 0 : displayable.hashCode());
    result = prime * result + ((gdnSku == null) ? 0 : gdnSku.hashCode());
    result = prime * result + ((merchantCode == null) ? 0 : merchantCode.hashCode());
    result = prime * result + ((minimumStock == null) ? 0 : minimumStock.hashCode());
    result = prime * result + ((off2OnActiveFlag == null) ? 0 : off2OnActiveFlag.hashCode());
    result = prime * result + ((price == null) ? 0 : price.hashCode());
    result = prime * result + ((requestId == null) ? 0 : requestId.hashCode());
    result = prime * result + ((salePrice == null) ? 0 : salePrice.hashCode());
    result = prime * result + ((stock == null) ? 0 : stock.hashCode());
    return result;
  }


  public void setBuyable(Boolean buyable) {
    this.buyable = buyable;
  }

  public void setDisplayable(Boolean displayable) {
    this.displayable = displayable;
  }

  public void setGdnSku(String gdnSku) {
    this.gdnSku = gdnSku;
  }

  public void setMerchantCode(String merchantCode) {
    this.merchantCode = merchantCode;
  }

  public void setMinimumStock(Integer minimumStock) {
    this.minimumStock = minimumStock;
  }

  public void setPrice(Double price) {
    this.price = price;
  }

  public void setRequestId(String requestId) {
    this.requestId = requestId;
  }

  public void setSalePrice(Double salePrice) {
    this.salePrice = salePrice;
  }

  public void setStock(Double stock) {
    this.stock = stock;
  }

  public Boolean getOff2OnActiveFlag() {
    return off2OnActiveFlag;
  }

  public void setOff2OnActiveFlag(Boolean off2OnActiveFlag) {
    this.off2OnActiveFlag = off2OnActiveFlag;
  }
  
  public String getUsername() {
	return username;
}

  public void setUsername(String username) {
    this.username = username;
  }

  public String getRemoteAddress() {
    return remoteAddress;
  }

  public void setRemoteAddress(String remoteAddress) {
    this.remoteAddress = remoteAddress;
  }

@Override
  public String toString() {
    return "BulkProcessApiUpdateProductQueue [requestId=" + requestId + ", merchantCode="
        + merchantCode + ", gdnSku=" + gdnSku + ", stock=" + stock + ", minimumStock="
        + minimumStock + ", price=" + price + ", salePrice=" + salePrice + ", buyable=" + buyable
        + ", displayable=" + displayable + ", off2OnActiveFlag=" + off2OnActiveFlag + "]";
  }
}
