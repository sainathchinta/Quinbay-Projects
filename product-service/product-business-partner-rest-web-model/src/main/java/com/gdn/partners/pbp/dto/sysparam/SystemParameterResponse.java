package com.gdn.partners.pbp.dto.sysparam;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.web.base.BaseResponse;

@JsonIgnoreProperties(ignoreUnknown = true)
public class SystemParameterResponse extends BaseResponse {
  private static final long serialVersionUID = 448758630537426049L;
  private Object value;

  public Object getValue() {
    return value;
  }

  public void setValue(Object value) {
    this.value = value;
  }

}
