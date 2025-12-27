package com.gdn.mta.bulk.dto;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.Date;

@Data
@AllArgsConstructor
@NoArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
@Builder
public class RecatProcessSummaryRequest {

   private Date requestStartDate;
   private Date requestEndDate;
   private String status;
   private String keyword;
   private String sortColumn;
   private String sortOrder;
}
