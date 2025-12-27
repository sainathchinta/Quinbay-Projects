package com.gda.mta.product.dto.response;

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
@JsonInclude
public class AutoApprovalTypeResponse extends BaseResponse {

  private String autoApprovalType;
  private String categoryCode;
  private String categoryName;

  public AutoApprovalTypeResponse(String autoApprovalType) {
    this.autoApprovalType = autoApprovalType;
  }
}
