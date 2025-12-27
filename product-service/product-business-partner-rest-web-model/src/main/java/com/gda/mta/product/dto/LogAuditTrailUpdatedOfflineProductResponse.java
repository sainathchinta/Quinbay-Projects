package com.gda.mta.product.dto;


import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.web.base.BaseResponse;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
@EqualsAndHashCode(callSuper = true)
@JsonIgnoreProperties(ignoreUnknown = true)
public class LogAuditTrailUpdatedOfflineProductResponse extends BaseResponse {
  private static final long serialVersionUID = 363587009186129977L;

  private String activity;
  private String oldValue;
  private String newValue;
  private String createdDateLog;
  private String createdByLog;
}
