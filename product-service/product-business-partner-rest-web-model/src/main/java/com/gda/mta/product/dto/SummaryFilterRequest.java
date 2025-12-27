package com.gda.mta.product.dto;

import java.util.ArrayList;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.mta.product.enums.StatusFilterType;
import com.gdn.mta.product.enums.TimeFilterType;
import com.gdn.partners.pbp.commons.util.SolrConstants;
import com.gdn.partners.pbp.commons.util.SolrFieldNames;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
@Builder
public class SummaryFilterRequest {

  private String assignedTo = SolrConstants.ALL;
  private String categoryCode =  SolrConstants.ALL;
  private String businessPartnerCode = SolrConstants.ALL;
  private String sortColumn = SolrFieldNames.SUBMITTED_DATE;
  private String sortOrder = SolrConstants.ASC;
  private String searchKeyword;
  private String timeFilter = TimeFilterType.ALL.getTimeFilterType();
  private String statusFilter = StatusFilterType.ALL.getStatusFilterType();
  private String suspensionStatus;
  private String sortType;
  private String nameKey;
  private List<String> pickupPointCodes = new ArrayList<>();
  private List<String> categoryCodes = new ArrayList<>();

}
