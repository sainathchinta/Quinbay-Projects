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
public class ReviewProductsFilterRequest {

  private String assignedTo;
  private String categoryCode;
  private String businessPartnerCode;
  private String sortColumn;
  private String sortOrder;
  private String searchKeyword;
  private String timeFilter;
  private String statusFilter;

}
