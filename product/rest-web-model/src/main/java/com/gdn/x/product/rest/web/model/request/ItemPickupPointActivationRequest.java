package com.gdn.x.product.rest.web.model.request;

import java.util.HashSet;
import java.util.Set;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class ItemPickupPointActivationRequest {
  private String pickupPointCode;
  private Set<PriceRequest> price;
  private Set<ItemViewConfigRequest> itemViewConfigs = new HashSet<ItemViewConfigRequest>();
  private boolean wholesalePriceExists = false;
  private boolean wholesalePriceActivated = false;
  private boolean cncActive;
  private boolean fbbActivated;
  private boolean distribution;
  private B2bFieldsRequest b2bFields;

  public ItemPickupPointActivationRequest(String pickupPointCode, Set<PriceRequest> price,
      Set<ItemViewConfigRequest> itemViewConfigs, boolean wholesalePriceExists, boolean wholesalePriceActivated,
      boolean cncActive) {
    this.pickupPointCode = pickupPointCode;
    this.price = price;
    this.itemViewConfigs = itemViewConfigs;
    this.wholesalePriceExists = wholesalePriceExists;
    this.wholesalePriceActivated = wholesalePriceActivated;
    this.cncActive = cncActive;
  }
}
