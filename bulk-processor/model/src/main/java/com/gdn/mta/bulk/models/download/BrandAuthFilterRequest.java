package com.gdn.mta.bulk.models.download;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class BrandAuthFilterRequest {

  private String sellerCode;
  private String brandName;
  private String status;
  private boolean brandAuthorise;
}