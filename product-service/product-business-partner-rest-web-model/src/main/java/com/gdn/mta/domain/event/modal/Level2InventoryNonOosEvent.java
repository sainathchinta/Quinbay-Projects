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
public class Level2InventoryNonOosEvent extends GdnBaseDomainEventModel {
  private String storeId;
  private String level2Id;
  private String level2MerchantCode;
  private String pickupPointCode;
  private String uniqueId;
  private boolean cncActivated;

  public Level2InventoryNonOosEvent(String storeId, String level2Id, String level2MerchantCode, String pickupPointCode) {
    this.storeId = storeId;
    this.level2Id = level2Id;
    this.level2MerchantCode = level2MerchantCode;
    this.pickupPointCode = pickupPointCode;
  }
}