package com.gdn.partners.pcu.internal.client.model.response;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.gdn.common.web.base.BaseResponse;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;

import java.util.Map;

@Data
@EqualsAndHashCode(callSuper = true)
@NoArgsConstructor
@AllArgsConstructor
@Builder
@JsonInclude(JsonInclude.Include.NON_NULL)
public class SystemParameterResponse extends BaseResponse {

  private static final long serialVersionUID = -6337221215922623776L;

  private Map<String, String> productSystemParameterSwitchValues;
}
