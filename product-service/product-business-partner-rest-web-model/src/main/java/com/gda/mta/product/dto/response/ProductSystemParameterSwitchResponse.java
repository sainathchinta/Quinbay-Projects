package com.gda.mta.product.dto.response;

import java.util.Map;

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
@JsonInclude
public class ProductSystemParameterSwitchResponse extends BaseResponse {


  private static final long serialVersionUID = 3110209897695888643L;

  private Map<String, Object> productSystemParameterSwitchValues;
}
