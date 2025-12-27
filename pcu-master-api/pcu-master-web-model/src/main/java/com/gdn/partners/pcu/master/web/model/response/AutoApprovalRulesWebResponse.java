package com.gdn.partners.pcu.master.web.model.response;

import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
@JsonInclude(JsonInclude.Include.ALWAYS)
@JsonIgnoreProperties(ignoreUnknown = true)
public class AutoApprovalRulesWebResponse {

  private String ruleName;
  private String autoApprovalType;
  private boolean markForDelete;
  private Integer sequenceNumber;
  private boolean restrictedKeyword;
  private boolean needRevisionConfigEnabled;
  private List<AutoApprovalRuleDetailWebResponse> ruleConfig;
  private List<AutoApprovalRuleDetailWebResponse> imageConfig;
  private List<AutoApprovalRuleDetailWebResponse> needRevisionImageConfig;
}
