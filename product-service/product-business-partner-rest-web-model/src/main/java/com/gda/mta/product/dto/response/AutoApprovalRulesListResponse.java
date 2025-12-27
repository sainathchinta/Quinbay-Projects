package com.gda.mta.product.dto.response;

import java.util.ArrayList;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.gda.mta.product.dto.AutoApprovalRulesDto;
import com.gdn.common.web.base.BaseResponse;
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
public class AutoApprovalRulesListResponse extends BaseResponse {
  private List<AutoApprovalRulesDto> autoApprovalRulesDtoList = new ArrayList<>();
}

