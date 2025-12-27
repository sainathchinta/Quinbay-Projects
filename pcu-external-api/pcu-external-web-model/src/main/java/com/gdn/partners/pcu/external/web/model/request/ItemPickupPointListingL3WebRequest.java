package com.gdn.partners.pcu.external.web.model.request;

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
public class ItemPickupPointListingL3WebRequest {
  private String businessPartnerCode;
  private String itemSku;
  private boolean needCorrection;
  private String keyword;
  private Set<String> pickupPointCodes;
}
