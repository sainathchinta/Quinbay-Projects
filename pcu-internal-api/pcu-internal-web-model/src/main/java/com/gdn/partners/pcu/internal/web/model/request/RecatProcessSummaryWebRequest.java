package com.gdn.partners.pcu.internal.web.model.request;

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
public class RecatProcessSummaryWebRequest {

  private String requestStartDate;
  private String requestEndDate;
  private String status;
  private String keyword;
  private String sortColumn;
  private String sortOrder;
}
