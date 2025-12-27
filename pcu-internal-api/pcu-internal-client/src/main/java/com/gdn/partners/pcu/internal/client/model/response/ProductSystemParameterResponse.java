package com.gdn.partners.pcu.internal.client.model.response;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.web.base.BaseResponse;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;


@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
public class ProductSystemParameterResponse extends BaseResponse {
  private String variable;
  private String value;
  private String description;
  private boolean showOnUI;
  private String variableNameEn;
  private String variableNameIn;
}
