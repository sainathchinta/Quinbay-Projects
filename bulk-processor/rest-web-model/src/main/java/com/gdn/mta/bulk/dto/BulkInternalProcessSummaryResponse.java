package com.gdn.mta.bulk.dto;

import java.util.Date;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.gdn.common.web.base.BaseResponse;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@JsonInclude
@Builder
public class BulkInternalProcessSummaryResponse extends BaseResponse {
  private String internalProcessRequestCode;
  private Date createdDate;
  private String status;
  private int totalCount;
  private Date endDate;
  private String initiator;
  private String sellerCode;
  private String sellerName;
  private String fileName;
  private String errorFilePath;
}
