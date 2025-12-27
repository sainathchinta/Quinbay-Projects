package com.gdn.partners.pcu.master.web.controller;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.partners.core.web.dto.SingleBaseResponse;
import com.gdn.partners.pcu.master.client.helper.ClientParameterHelper;
import com.gdn.partners.pcu.master.model.AutoQcConfigApiPath;
import com.gdn.partners.pcu.master.service.AutoQcConfigService;
import com.gdn.partners.pcu.master.web.model.request.AutoQcConfigUpdateWebRequest;
import com.gdn.partners.pcu.master.web.model.response.AutoApprovalRulesListWebResponse;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.extern.slf4j.Slf4j;

@Slf4j
@Tag(name = "AutoQcConfig API")
@RestController
@RequestMapping(value = AutoQcConfigApiPath.BASE_PATH)
public class AutoQcConfigController {

  @Autowired
  private AutoQcConfigService autoQcConfigService;

  @Autowired
  private ClientParameterHelper clientParameterHelper;

  @Operation(summary = "update auto qc config rule")
  @PutMapping(value = AutoQcConfigApiPath.UPDATE_AUTO_APPROVAL_RULES_BY_RULE_NAME, produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  public GdnBaseRestResponse updateAutoQcConfigRule(
      @RequestParam(required = false, defaultValue = "false") boolean isNeedRevisionConfig,
      @PathVariable("ruleName") String ruleName, @RequestBody AutoQcConfigUpdateWebRequest autoQcConfigUpdateWebRequest)
      throws Exception {
    GdnBaseRestResponse response = autoQcConfigService.updateAutoQcConfigRule(ruleName, isNeedRevisionConfig, autoQcConfigUpdateWebRequest);
    return response;
  }

  @Operation(summary = "getting auto approval rules")
  @GetMapping(value = AutoQcConfigApiPath.GET_AUTO_APPROVAL_RULES, produces = MediaType.APPLICATION_JSON_VALUE)
  public SingleBaseResponse<AutoApprovalRulesListWebResponse> getAutoApprovalRules() throws Exception {
    log.info("get Auto approval rules");
    AutoApprovalRulesListWebResponse response = autoQcConfigService.getAutoApprovalRules();
    return new SingleBaseResponse<>(null, null, true, clientParameterHelper.getRequestId(),
        response);
  }
}