package com.gdn.mta.product.entity;

import java.io.Serializable;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
@Builder
public class ItemFlagDetails implements Serializable {

  private static final long serialVersionUID = -8228554434009593849L;

  private String itemSku;
  private String pickupPointCode;
  private boolean buyable;
  private boolean displayable;
  private boolean cncBuyable;
  private boolean cncDisplayable;
  private String productItemId;

  public ItemFlagDetails(String itemSku, String pickupPointCode, boolean buyable, boolean displayable,
      String productItemId) {
    this.itemSku = itemSku;
    this.pickupPointCode = pickupPointCode;
    this.buyable = buyable;
    this.displayable = displayable;
    this.productItemId = productItemId;
  }
}