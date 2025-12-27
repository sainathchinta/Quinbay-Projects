package com.gdn.mta.bulk.dto;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.gdn.mta.bulk.BulkInternalProcessType;
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
public class BulkInternalUploadRequestDTO {
  private String internalProcessRequestCode;
  private String relativePath;
  private String fileName;
  private BulkInternalProcessType bulkInternalProcessType;
  private String businessPartnerCode;
  private String createdBy;
  private boolean instore;
}
