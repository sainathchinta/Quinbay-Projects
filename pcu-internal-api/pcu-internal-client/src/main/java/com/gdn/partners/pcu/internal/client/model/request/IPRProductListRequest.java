package com.gdn.partners.pcu.internal.client.model.request;

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
public class IPRProductListRequest {

  private String keyword;
  private String timeFilterWebType;
  private String state;
  private String businessPartnerCode;
  private String categoryCode;
  private String brandCode;
  private String sortOrder;
  private String assignedTo;
  private Boolean assigned;
}