package com.gdn.mta.domain.event.modal;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.entity.GdnBaseDomainEventModel;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class Level2InventoryMinimumStockAlertEvent extends GdnBaseDomainEventModel {

  private String storeId;
  private String gdnSku;
  private Integer availableStock;
  private Integer minimumStock;
  private String businessPartnerCode;
  private String pickupPointCode;
}
