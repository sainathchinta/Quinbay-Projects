package com.gda.mta.product.dto;

import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
@JsonInclude
@JsonIgnoreProperties(ignoreUnknown = true)
public class ProductL3ListingRequest {

  private String merchantCode;
  private String keyword;
  private List<String> categoryCodes;
  private List<String> pickupPointCodes;
  private List<String> promoTypes;
  private String sortField;
  private String sortOrder;
  private Boolean inStock;
  private Boolean archived;
  private Boolean suspended;
}
