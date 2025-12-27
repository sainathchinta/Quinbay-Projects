package com.gdn.mta.bulk.dto;

import java.util.Date;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
@Builder
public class BulkInternalProcessSummaryRequest {
  private Date startDate;
  private Date endDate;
  private String status;
  private String keyword;
  private String processType;
  private String sortColumn;
  private String sortOrder;
}
