package com.gdn.partners.pcu.external.web.model.request;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.ArrayList;
import java.util.List;

@Data
@Builder
@JsonInclude
@JsonIgnoreProperties(ignoreUnknown = true)
@AllArgsConstructor
@NoArgsConstructor
public class ProductL3ListingWebRequest {
  private String searchKey;
  private List<String> categoryCodes = new ArrayList<>();
  private List<String> pickupPointCodes = new ArrayList<>();
  private Boolean cncActivated;
  private Boolean tradingProduct;
  private Boolean bundlingProduct;
}
