package com.gdn.x.product.rest.web.model.response;

import java.util.Map;

import com.gdn.common.web.base.BaseResponse;
import java.util.List;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.ToString;

@Data
@AllArgsConstructor
@NoArgsConstructor
@ToString
public class ProductScoreRuleResponse extends BaseResponse {
  private String categoryCode;
  private Map<String, MaxScoreAndRuleConfigResponse> productScoreRules;
  private List<String> ignoreSymbols;
  private List<IgnoreAttributeSet> ignoreAttributes;
}
