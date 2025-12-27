package com.gdn.mta.bulk.dto;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.gdn.common.web.base.BaseResponse;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.Date;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
public class BulkProcessStatusListingResponse extends BaseResponse {
  private static final long serialVersionUID = 3307268417772364871L;
  String businessPartnerCode;
  String bulkProcessType;
  Date uploadDate;
  String uploadedFileName;
  @JsonProperty("isProcessCompleted")
  boolean isProcessCompleted;
  int totalRowCountRequested;
  int errorRowCount;
  int successRowCount;
  double processCompletionPercentage;
  BulkActivityStatus bulkActivityStatus;
  Date estimatedCompletionTime;
  String errorFileLink;
  String bulkProcessCode;
  String user;
  String downloadFileLink;
  String description;
}
