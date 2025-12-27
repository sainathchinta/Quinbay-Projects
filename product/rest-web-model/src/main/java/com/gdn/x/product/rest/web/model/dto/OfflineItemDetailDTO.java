package com.gdn.x.product.rest.web.model.dto;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

import java.io.Serializable;
import java.util.Set;

@JsonIgnoreProperties(ignoreUnknown = true)
public class OfflineItemDetailDTO implements Serializable {
  private String uniqueId;
  private String itemSku;
  private String pickupPointCode;
  private Set<PriceDTO> prices;
  private Set<ItemViewConfigDTO> itemViewConfigs;

  public OfflineItemDetailDTO() {
  }

  @Override
  public boolean equals(Object obj) {
    return super.equals(obj);
  }

  public String getItemSku() {
    return itemSku;
  }

  public Set<ItemViewConfigDTO> getItemViewConfigs() {
    return itemViewConfigs;
  }

  public String getPickupPointCode() {
    return pickupPointCode;
  }

  public Set<PriceDTO> getPrices() {
    return prices;
  }

  public String getUniqueId() {
    return uniqueId;
  }

  @Override
  public int hashCode() {
    return super.hashCode();
  }

  public void setItemSku(String itemSku) {
    this.itemSku = itemSku;
  }

  public void setPickupPointCode(String pickupPointCode) {
    this.pickupPointCode = pickupPointCode;
  }

  public void setPrices(Set<PriceDTO> prices) {
    this.prices = prices;
  }

  public void setItemViewConfigs(Set<ItemViewConfigDTO> itemViewConfigs) {
    this.itemViewConfigs = itemViewConfigs;
  }

  public void setUniqueId(String uniqueId) {
    this.uniqueId = uniqueId;
  }

  @Override
  public String toString() {
    final StringBuilder sb = new StringBuilder("OfflineItemDetailDTO{");
    sb.append("uniqueId='").append(uniqueId).append('\'');
    sb.append(", itemSku='").append(itemSku).append('\'');
    sb.append(", pickupPointCode='").append(pickupPointCode).append('\'');
    sb.append(", prices=").append(prices);
    sb.append(", itemViewConfigs=").append(itemViewConfigs);
    sb.append('}');
    return sb.toString();
  }
}
