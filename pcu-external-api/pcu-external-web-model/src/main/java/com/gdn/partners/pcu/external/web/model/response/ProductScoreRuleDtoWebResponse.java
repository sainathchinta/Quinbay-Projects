package com.gdn.partners.pcu.external.web.model.response;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.ToString;
import java.util.List;

@Data
@NoArgsConstructor
@AllArgsConstructor
@ToString
public class ProductScoreRuleDtoWebResponse {
  private int maxScore;
  private List<RuleConfigWebResponse> ruleConfig;
}
