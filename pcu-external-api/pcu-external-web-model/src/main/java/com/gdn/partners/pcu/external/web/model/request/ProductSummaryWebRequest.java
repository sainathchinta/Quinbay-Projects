package com.gdn.partners.pcu.external.web.model.request;

import java.util.List;

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
public class ProductSummaryWebRequest {

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
  private Boolean b2cActivated;
  private Boolean b2bActivated;
}
