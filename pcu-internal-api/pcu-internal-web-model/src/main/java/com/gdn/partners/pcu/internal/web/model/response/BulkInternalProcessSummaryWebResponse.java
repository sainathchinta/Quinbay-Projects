package com.gdn.partners.pcu.internal.web.model.response;

import java.util.Date;

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
public class BulkInternalProcessSummaryWebResponse {
  private String internalProcessRequestCode;
  private Date createdDate;
  private String status;
  private int totalCount;
  private Date endDate;
  private String initiator;
  private String sellerCode;
  private String sellerName;
  private String filePath;
  private String fileName;
  private String errorFilePath;
}
