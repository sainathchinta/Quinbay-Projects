package com.gdn.mta.bulk.dto;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@JsonInclude
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class BulkInternalProcessUploadRequest {

  private String internalProcessRequestCode;
  private String sellerCode;
  private String sellerName;
  private String fileName;
  private String processType;
  private String notes;
}
