package com.gdn.mta.bulk.dto;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.gdn.common.web.base.BaseResponse;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.Date;

@Data
@AllArgsConstructor
@NoArgsConstructor
@JsonInclude
@Builder
public class RecatProcessSummaryResponse extends BaseResponse {

  private String recatRequestCode;
  private Date uploadDate;
  private Date scheduledDate;
  private String status;
  private int productCount;
  private Date completionDate;
  private String initiator;
}
