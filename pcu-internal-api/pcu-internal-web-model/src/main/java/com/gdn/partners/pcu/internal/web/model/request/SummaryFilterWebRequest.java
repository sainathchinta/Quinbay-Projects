package com.gdn.partners.pcu.internal.web.model.request;

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
public class SummaryFilterWebRequest {
  private String keyword;
  private String categoryCode;
  private String businessPartnerCode;
  private Boolean reviewPending;
  private boolean activated;
  private boolean viewable;
  private String sortBy;
  private String status;
  private String startDate;
  private String endDate;
  private String timeFilterType;
}
