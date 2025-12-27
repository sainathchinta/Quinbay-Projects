package com.gda.mta.product.dto;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.gdn.common.web.base.BaseResponse;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
@JsonInclude()
public class LogAuditTrailUpdatedProductResponse extends BaseResponse {

  private static final long serialVersionUID = 4966199123664714547L;

  private String activity;
  private String oldValue;
  private String newValue;
  private String createdDateLog;
  private String createdByLog;
}
