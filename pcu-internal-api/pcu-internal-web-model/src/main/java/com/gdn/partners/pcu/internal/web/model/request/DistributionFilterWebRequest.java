package com.gdn.partners.pcu.internal.web.model.request;

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
public class DistributionFilterWebRequest {
  private String keyword;
  private String categoryCode;
  private String businessPartnerCode;
  private List<Integer> rejectedList;
  private List<String> vendorCodes;
  private List<String> statusList;
  private String startDate;
  private String endDate;
  private String sortBy;
  private String timeFilterType;
}
