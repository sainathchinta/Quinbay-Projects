package com.gdn.partners.pcu.external.web.model.response;


import java.util.Date;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
@JsonInclude
@JsonIgnoreProperties(ignoreUnknown = true)
public class LogAuditTrailUpdatedProductWebResponse {

  private String activity;
  private String oldValue;
  private String newValue;
  private Date createdDateLog;
  private String createdByLog;
}
