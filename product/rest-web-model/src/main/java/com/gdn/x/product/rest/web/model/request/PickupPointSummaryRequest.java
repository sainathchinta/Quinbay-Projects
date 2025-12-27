package com.gdn.x.product.rest.web.model.request;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.Set;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class PickupPointSummaryRequest {
  private String businessPartnerCode;
  private String productSku;
  private String keyword;
  private Boolean cncActivated;
  private Boolean fbbActivated;
  private Set<String> codes;
}
