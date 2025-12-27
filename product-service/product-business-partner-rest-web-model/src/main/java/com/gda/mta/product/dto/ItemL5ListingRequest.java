package com.gda.mta.product.dto;

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
public class ItemL5ListingRequest {
  private String businessPartnerCode;
  private String itemSku;
  private String productSku;
  private String keyword;
  private Set<String> pickupPointCodes;
}
