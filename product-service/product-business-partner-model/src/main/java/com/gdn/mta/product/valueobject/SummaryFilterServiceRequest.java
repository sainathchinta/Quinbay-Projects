package com.gdn.mta.product.valueobject;

import jakarta.persistence.EnumType;
import jakarta.persistence.Enumerated;

import com.gdn.mta.product.enums.StatusFilterType;
import com.gdn.mta.product.enums.TimeFilterType;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class SummaryFilterServiceRequest {

  private String assignedTo;
  private String categoryCode;
  private String businessPartnerCode;
  private String sortColumn;
  private String sortOrder;
  private String searchKeyword;
  private TimeFilterType timeFilter;
  private StatusFilterType statusFilter;
}
