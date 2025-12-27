package com.gdn.partners.pcu.internal.client.model.request;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
@JsonInclude
@JsonIgnoreProperties(ignoreUnknown = true)
public class BrandAuthFilterRequest {
  private String sellerCode;
  private String brandName;
  private String status;
  private boolean brandAuthorise;
}
