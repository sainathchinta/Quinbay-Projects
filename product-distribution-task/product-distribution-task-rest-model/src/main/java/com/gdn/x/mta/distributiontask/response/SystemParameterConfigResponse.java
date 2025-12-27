package com.gdn.x.mta.distributiontask.response;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.web.base.BaseResponse;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@JsonIgnoreProperties(ignoreUnknown = true)
@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class SystemParameterConfigResponse extends BaseResponse {

  private static final long serialVersionUID = -5637556344123224946L;
  private String variable;
  private String value;
  private String description;
}
