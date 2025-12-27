package com.gda.mta.product.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class InventoryUpsertModel {
  private String businessPartnerCode;
  private String itemCode;
  private String productSku;
  private String itemSku;
  private String oldPickupPointCode;
  private String newPickupPointCode;
  private Integer stock;
  private Integer minimumStock;
  private Boolean syncStock;
  private Boolean fbbActive;
  private boolean distribution;
}
