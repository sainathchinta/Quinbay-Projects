package com.gdn.x.product.rest.web.model.request;

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
public class ItemPickupPointListingRequest {
  private String businessPartnerCode;
  private String productSku;
  private String itemSku;
  private String keyword;
  private boolean responseWithoutPickupPoint;
  private Set<String> pickupPointCodes;
  private boolean isFbbSortRequired;
}
