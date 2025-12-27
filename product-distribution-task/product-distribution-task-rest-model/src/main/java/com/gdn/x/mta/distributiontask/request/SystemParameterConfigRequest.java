package com.gdn.x.mta.distributiontask.request;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.web.base.BaseRequest;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@JsonIgnoreProperties(ignoreUnknown = true)
@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class SystemParameterConfigRequest extends BaseRequest {

  private static final long serialVersionUID = -3412645974900523686L;
  private String variable;
  private String value;
  private String description;
}