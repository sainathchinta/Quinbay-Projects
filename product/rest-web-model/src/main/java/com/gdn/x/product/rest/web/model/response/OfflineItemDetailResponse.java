package com.gdn.x.product.rest.web.model.response;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.web.base.BaseResponse;
import com.gdn.x.product.rest.web.model.dto.ItemViewConfigDTO;
import com.gdn.x.product.rest.web.model.dto.PriceDTO;

import java.util.Set;

/**
 * Created by william.s.setiadi on 4/16/2018.
 */
@JsonIgnoreProperties(ignoreUnknown = true)
public class OfflineItemDetailResponse extends BaseResponse {

  private String uniqueId;
  private String pickupPointCode;
  private Set<PriceDTO> prices;
  private Set<ItemViewConfigDTO> itemViewConfigs;

  public OfflineItemDetailResponse() {
  }

  public OfflineItemDetailResponse(String uniqueId, String pickupPointCode, Set<PriceDTO> prices,
      Set<ItemViewConfigDTO> itemViewConfigs) {
    this.uniqueId = uniqueId;
    this.pickupPointCode = pickupPointCode;
    this.prices = prices;
    this.itemViewConfigs = itemViewConfigs;
  }

  public String getUniqueId() {
    return uniqueId;
  }

  public void setUniqueId(String uniqueId) {
    this.uniqueId = uniqueId;
  }

  public String getPickupPointCode() {
    return pickupPointCode;
  }

  public void setPickupPointCode(String pickupPointCode) {
    this.pickupPointCode = pickupPointCode;
  }

  public Set<PriceDTO> getPrices() {
    return prices;
  }

  public void setPrices(Set<PriceDTO> prices) {
    this.prices = prices;
  }

  public Set<ItemViewConfigDTO> getItemViewConfigs() {
    return itemViewConfigs;
  }

  public void setItemViewConfigs(Set<ItemViewConfigDTO> itemViewConfigs) {
    this.itemViewConfigs = itemViewConfigs;
  }

  @Override
  public String toString() {
    final StringBuilder sb = new StringBuilder("OfflineItemDetailResponse{");
    sb.append("uniqueId='").append(uniqueId).append('\'');
    sb.append(", pickupPointCode='").append(pickupPointCode).append('\'');
    sb.append(", prices=").append(prices);
    sb.append(", itemViewConfigs=").append(itemViewConfigs);
    sb.append('}');
    return sb.toString();
  }
}
