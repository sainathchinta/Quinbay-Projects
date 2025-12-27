package com.gda.mta.product.dto;

import java.io.Serializable;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
@JsonInclude

public class ItemSkuPickupPointSyncStockDto implements Serializable {
  private static final long serialVersionUID = -4966102020470355850L;
  private String itemSku;
  private String pickupPointCode;
  private boolean syncStock;
  private Integer stock;

  public ItemSkuPickupPointSyncStockDto(String itemSku, String pickupPointCode, boolean syncStock) {
    this.itemSku = itemSku;
    this.pickupPointCode = pickupPointCode;
    this.syncStock = syncStock;
  }
}
