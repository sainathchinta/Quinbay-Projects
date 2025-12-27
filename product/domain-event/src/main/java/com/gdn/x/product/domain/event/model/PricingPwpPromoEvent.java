package com.gdn.x.product.domain.event.model;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.gdn.common.base.entity.GdnBaseDomainEventModel;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
public class PricingPwpPromoEvent extends GdnBaseDomainEventModel {

  private String merchantCode;
  private String itemSku;
  private String pickupPointCode;
  private String itemPickupPointId;
  @JsonProperty("isPartOfPendingPWPAsMain")
  private Boolean isPartOfPendingPWPAsMain;

  @JsonProperty("isPartOfPendingPWPAsAdditional")
  private Boolean isPartOfPendingPWPAsAdditional;

  @JsonProperty("isPartOfActivePWPAsMain")
  private Boolean isPartOfActivePWPAsMain;

  @JsonProperty("isPartOfActivePWPAsAdditional")
  private Boolean isPartOfActivePWPAsAdditional;

  private String storeId;
}
