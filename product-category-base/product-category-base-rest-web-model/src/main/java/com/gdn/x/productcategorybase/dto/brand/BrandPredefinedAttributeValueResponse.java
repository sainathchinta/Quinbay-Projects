package com.gdn.x.productcategorybase.dto.brand;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.gdn.common.web.base.BaseResponse;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
@JsonInclude(JsonInclude.Include.ALWAYS)
public class BrandPredefinedAttributeValueResponse extends BaseResponse {

  private static final long serialVersionUID = 7271001994192470256L;
  private String predefinedAllowedAttributeCode;
  private String value;
  private Integer sequence;
  private String brandApprovalStatus;
  private String brandRequestCode;
}
