package com.gdn.x.productcategorybase.dto.response;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.gdn.common.web.base.BaseResponse;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@JsonInclude
@Builder
public class BatchVatUpdateResponse extends BaseResponse {

  private String skuCode;
  private String vatFlagRequest;
  private String errorMessage;
}
