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
public class RetryAutoNeedRevisionResponse extends BaseResponse {

  private static final long serialVersionUID = -4967621787340571303L;
  private String productCode;
  private boolean isProductActive = true;
  private String state;
  private String reason;
  private boolean success = true;
  private String imageReason;
}
