package com.gdn.partners.pcu.internal.web.model.response;

import com.fasterxml.jackson.annotation.JsonInclude;
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
public class RecatProcessSummaryWebResponse {

  private String recatRequestCode;
  private Date uploadDate;
  private Date scheduledDate;
  private String status;
  private int productCount;
  private Date completionDate;
  private String initiator;
}
