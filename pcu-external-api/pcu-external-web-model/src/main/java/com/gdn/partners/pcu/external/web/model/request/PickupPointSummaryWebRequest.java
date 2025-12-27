package com.gdn.partners.pcu.external.web.model.request;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.HashSet;
import java.util.Set;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class PickupPointSummaryWebRequest {
  private String businessPartnerCode;
  private String productSku;
  private String keyword;
  private Boolean cncActivated;
  private Boolean defaultAddress;
  private String sortDirection;
  private String sortedBy;
  private Boolean fbbActivated;
  private Set<String> pickupPointCodes = new HashSet<>();
}
