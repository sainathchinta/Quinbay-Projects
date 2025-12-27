package com.gdn.mta.bulk.models.download.responsedata;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
@JsonInclude(JsonInclude.Include.NON_NULL)
public class BulkAddReviewIPRProductsRequestData {
  private String productSku;
  private String productName;
  private String action;
  private String reasons;
  private String sellerNotes;
  private String reviewerNotes;
  private String assignee;
  private String violationType;
  private String source;
  private int excelRowNumber;
  private String reportDate;
  private String reporter;
  private String reporterName;
  private String reporterPhone;
  private String reporterEmail;
  private String reporterAddress;
  private String reporterReason;
}
