package com.gdn.partners.pbp.dto.offlineitem;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.web.base.BaseResponse;

@JsonIgnoreProperties(ignoreUnknown = true)
public class OfflineItemDetailResponse extends BaseResponse {

  private static final long serialVersionUID = 6949481516846728978L;

  private String pickupPointCode;
  private String pickupPointName;
  private Integer originalStock;
  private Integer stock;
  private Double listPrice;
  private Double price;
  private boolean active;

  public String getPickupPointCode() {
    return pickupPointCode;
  }

  public String getPickupPointName() {
    return pickupPointName;
  }

  public Double getListPrice() {
    return listPrice;
  }

  public Double getPrice() {
    return price;
  }

  public Integer getOriginalStock() {
    return originalStock;
  }

  public Integer getStock() {
    return stock;
  }

  public boolean isActive() {
    return active;
  }

  public void setActive(boolean active) {
    this.active = active;
  }

  public void setPickupPointCode(String pickupPointCode) {
    this.pickupPointCode = pickupPointCode;
  }

  public void setPickupPointName(String pickupPointName) {
    this.pickupPointName = pickupPointName;
  }

  public void setListPrice(Double listPrice) {
    this.listPrice = listPrice;
  }

  public void setPrice(Double price) {
    this.price = price;
  }

  public void setOriginalStock(Integer originalStock) {
    this.originalStock = originalStock;
  }

  public void setStock(Integer stock) {
    this.stock = stock;
  }

  @Override
  public String toString() {
    final StringBuilder sb = new StringBuilder("OfflineItemDetailResponse{");
    sb.append("pickupPointCode='").append(pickupPointCode).append('\'');
    sb.append(", pickupPointName='").append(pickupPointName).append('\'');
    sb.append(", originalStock=").append(originalStock);
    sb.append(", stock=").append(stock);
    sb.append(", listPrice=").append(listPrice);
    sb.append(", price=").append(price);
    sb.append(", active=").append(active);
    sb.append('}');
    return sb.toString();
  }
}
