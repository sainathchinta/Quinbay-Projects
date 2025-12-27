package com.gda.mta.product.dto;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.gdn.common.web.base.BaseResponse;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
@JsonInclude
public class NeedRevisionEligibilityResponse extends BaseResponse {
  private String productCode;
  private Boolean eligibleForDeletion;
  private String productName;
  private boolean partOfCampaign;
  private boolean activeOrderHistory;
}
