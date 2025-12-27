package com.gdn.mta.product.controller;


import java.util.ArrayList;
import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.bind.annotation.RestController;

import com.gda.mta.product.dto.AutoQcConfigChangeRequest;
import com.gda.mta.product.dto.AutoQcConfigRequest;
import com.gda.mta.product.dto.ProductImagePredictionResponse;
import com.gda.mta.product.dto.response.AutoApprovalRulesListResponse;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.mta.product.entity.AutoApprovalRules;
import com.gdn.mta.product.service.AutoApprovalService;
import com.gdn.mta.product.service.AutoQcConfigService;
import com.gdn.mta.product.service.ErrorMessages;
import com.gdn.mta.product.service.ProductImagePredictionService;
import com.gdn.mta.product.service.exception.ApiInvalidImageQcConfigException;
import com.gdn.mta.product.util.ConverterUtil;
import com.gdn.mta.product.util.SingleValueResponse;
import com.gdn.mta.product.web.model.AutoQcConfigControllerPath;
import io.swagger.v3.oas.annotations.tags.Tag;
import io.swagger.v3.oas.annotations.Operation;
import lombok.extern.slf4j.Slf4j;

@Slf4j
@RestController
@RequestMapping(value = AutoQcConfigControllerPath.BASE_PATH)
@Tag(name = "AutoQcConfigController", description = "AutoQcConfig Service API")
public class AutoQcConfigController {

  @Autowired
  private AutoQcConfigService autoQcConfigService;

  @Autowired
  private AutoApprovalService autoApprovalService;

  @Autowired
  private ProductImagePredictionService productImagePredictionService;

  @RequestMapping(value = AutoQcConfigControllerPath.UPDATE_AUTO_QC_RULE, method = RequestMethod.PUT, consumes = MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "update Auto Approval Rules", description = "update Auto approval rules")
  @ResponseBody
  public GdnBaseRestResponse update(@RequestParam String storeId, @RequestParam String channelId,
      @RequestParam String clientId, @RequestParam String requestId, @RequestParam String username,
      @RequestParam(required = false, defaultValue = "false") boolean isNeedRevisionConfig,
      @PathVariable String ruleName, @RequestBody AutoQcConfigRequest autoQcConfigRequest) {
    try {
      autoQcConfigService.update(ruleName, storeId, autoQcConfigRequest, isNeedRevisionConfig);
      return new GdnBaseRestResponse(null, null, true, requestId);
    } catch (ApiInvalidImageQcConfigException e) {
      log.error(
          "updateAutoQcConfig : failed to update AutoApprovalRules , errorCode : {}, errorMessage : {}, exceptionType : {}, requestId : {},autoQcConfigRequest : {}",
          e.getErrorCode(), e.getErrorMsg(), e.getExceptionType(), requestId, autoQcConfigRequest);
      return new GdnBaseRestResponse(e.getErrorMsg(), e.getErrorCode().toString(), Boolean.FALSE, requestId);
    } catch (Exception e) {
      log.error("updateAutoQcConfig : failed to update AutoApprovalRules , requestId : {},autoQcConfigRequest : {}",
          requestId, autoQcConfigRequest, e);
      return new GdnBaseRestResponse(e.getMessage(), e.getMessage(), Boolean.FALSE, requestId);
    }
  }

  @RequestMapping(value = AutoQcConfigControllerPath.GET_AUTO_QC_RULES, method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "get all auto Qc config rules", description = "get all auto Qc config rules")
  @ResponseBody
  public GdnRestSingleResponse<AutoApprovalRulesListResponse> getAllAutoQcConfigRules(@RequestParam String storeId,
      @RequestParam String username, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId) {
    try {
      List<AutoApprovalRules> autoApprovalRules = autoApprovalService.findAllAutoQcConfigRules();
      List<ProductImagePredictionResponse> imagePredictionResponseList =
          productImagePredictionService.findByStoreId(storeId);
      AutoApprovalRulesListResponse autoApprovalRulesListResponse =
          ConverterUtil.toAutoApprovalRulesListResponse(autoApprovalRules, imagePredictionResponseList);
      return new GdnRestSingleResponse<AutoApprovalRulesListResponse>(null, null, true, autoApprovalRulesListResponse,
          requestId);
    } catch (Exception e) {
      log.error(ErrorMessages.ERROR_WHILE_FETCHING_ATUO_QC_RULES, e);
      return new GdnRestSingleResponse<AutoApprovalRulesListResponse>(e.getMessage(), e.getMessage(), false, null,
          requestId);
    }
  }

  @RequestMapping(value = AutoQcConfigControllerPath.VERIFY_RULE_CONFIG_CHANGE, method = RequestMethod.POST, consumes = MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "verify rule config change", description = "verify rule config change")
  @ResponseBody
  public GdnRestSingleResponse<SingleValueResponse> verifyAutoQcConfigChange(@RequestParam String storeId, @RequestParam String channelId,
      @RequestParam String clientId, @RequestParam String requestId, @RequestParam String username,
      @RequestBody AutoQcConfigChangeRequest autoQcConfigChangeRequest) {
    try {
      boolean isConfigChanged =
          autoApprovalService.verifyAutoApprovalRulesForConfigChange(storeId, autoQcConfigChangeRequest);
      return new GdnRestSingleResponse(null, null, true, new SingleValueResponse(String.valueOf(isConfigChanged)),
          requestId);
    } catch (Exception e) {
      log.error("failed to verify rule config change , requestId : {}, autoQcConfigVhangeRequest : {}", requestId,
          autoQcConfigChangeRequest, e);
      return new GdnRestSingleResponse<>(e.getMessage(), e.getMessage(), Boolean.FALSE, null, requestId);
    }
  }
}
