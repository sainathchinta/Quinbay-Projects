package com.gdn.x.product.model.vo;

import java.util.HashMap;
import java.util.Map;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.gdn.common.web.base.BaseRequest;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
@JsonInclude
public class SimpleStringBooleanMapRequest extends BaseRequest {

  private static final long serialVersionUID = -5547593224454295509L;

  private Map<String, Boolean> stringBooleanMap = new HashMap<String, Boolean>();
}
