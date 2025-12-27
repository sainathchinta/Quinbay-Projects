package com.gdn.partners.pcu.external.web.model.response;

import java.util.Map;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.ToString;
import java.util.List;

@Data
@NoArgsConstructor
@AllArgsConstructor
@ToString
public class ProductScoreRuleWebResponse extends BaseResponse{
  private String categoryCode;
  private Map<String, ProductScoreRuleDtoWebResponse> productScoreRules;
  private List<String> ignoreSymbols;
  private List<IgnoreAttributeWebResponse> ignoreAttributes;
}
