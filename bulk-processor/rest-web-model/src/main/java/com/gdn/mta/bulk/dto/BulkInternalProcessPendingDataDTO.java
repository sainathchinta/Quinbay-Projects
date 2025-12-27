package com.gdn.mta.bulk.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@AllArgsConstructor
@NoArgsConstructor
@Data
@Builder
public class BulkInternalProcessPendingDataDTO {

  private String internalProcessRequestId;
  private String parentCode;
  private String processType;
}
