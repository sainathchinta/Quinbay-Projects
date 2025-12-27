package com.gda.mta.product.dto;

import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.gda.mta.product.dto.response.AutoApprovalRuleDetailsDto;
import com.gdn.common.web.base.BaseResponse;
import com.gdn.mta.product.enums.AutoApprovalType;
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
public class AutoApprovalRulesDto extends BaseResponse {
  private Integer sequenceNumber;
  private String ruleName;
  private List<AutoApprovalRuleDetailsDto> ruleConfig;
  private List<AutoApprovalRuleDetailsDto> imageQcConfig;
  private List<AutoApprovalRuleDetailsDto> needRevisionImageQcConfig;
  private AutoApprovalType autoApprovalType;
  private boolean needRevisionConfigEnabled;
  private boolean markForDelete;
}
