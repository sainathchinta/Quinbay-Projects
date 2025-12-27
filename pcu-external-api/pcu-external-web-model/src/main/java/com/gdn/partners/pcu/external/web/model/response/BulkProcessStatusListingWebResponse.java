package com.gdn.partners.pcu.external.web.model.response;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.gdn.common.web.base.BaseResponse;
import com.gdn.partners.pcu.external.web.BulkActivityStatusWeb;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.Date;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class BulkProcessStatusListingWebResponse extends BaseResponse {
  private static final long serialVersionUID = 1593162098397185873L;
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
  BulkActivityStatusWeb status;
  Date estimatedCompletionTime;
  String errorFileLink;
  String bulkProcessCode;
  String user;
  String downloadFileLink;
  String description;
}
