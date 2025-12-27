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
public class BulkAssigneeMasterSkuReviewRequestData {
  private String firstAnchorSku;
  private String firstAnchorSkuName;
  private String secondAnchorSku;
  private String secondAnchorSkuName;
  private String assignee;
  private int rowNumber;
}
