package com.gdn.x.product.rest.web.model.response;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.ToString;
import java.util.List;

@Data
@NoArgsConstructor
@AllArgsConstructor
@ToString
public class MaxScoreAndRuleConfigResponse {
  private int maxScore;
  private List<RuleConfigResponse> ruleConfig;
}
