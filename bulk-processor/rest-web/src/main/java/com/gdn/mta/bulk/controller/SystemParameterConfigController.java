package com.gdn.mta.bulk.controller;

import com.gdn.mta.bulk.service.util.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;

import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.mta.bulk.dto.SystemParameterConfigRequest;
import com.gdn.mta.bulk.dto.SystemParameterConfigResponse;
import com.gdn.mta.bulk.entity.SystemParameterConfig;
import com.gdn.mta.bulk.logger.LoggerAttributeModel;
import com.gdn.mta.bulk.logger.LoggerStandard;
import com.gdn.mta.bulk.service.SystemParameterConfigService;

import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.extern.slf4j.Slf4j;

@Slf4j
@Controller
@RequestMapping(value = SystemParameterConfigController.BASE_PATH)
@Tag(name = "SystemParameterConfigController", description = "SystemParamenterConfig API")
public class SystemParameterConfigController {
  
  public static final String BASE_PATH = "/api/system-parameters";
  public static final String SYSTEM_PARAMETER_DELETE = "/delete";
  public static final String SYSTEM_PARAMETER_FIND_ONE = "/find-one";
  public static final String SYSTEM_PARAMETER_ADD = "/add";
  public static final String SYSTEM_PARAMETER_UPDATE = "/update";

  @Autowired
  private SystemParameterConfigService systemParameterConfigService;

  @RequestMapping(value = {SystemParameterConfigController.SYSTEM_PARAMETER_DELETE}, method = {RequestMethod.DELETE},
      produces = {MediaType.APPLICATION_JSON_VALUE})
  @ResponseBody
  public GdnBaseRestResponse deleteSystemParameter(@RequestParam String storeId,
      @RequestParam String requestId, @RequestParam String channelId,
      @RequestParam String clientId, @RequestParam(required = false) String username, @RequestParam String variable) {
    LoggerAttributeModel loggerModel =
        new LoggerAttributeModel(this, "deleteSystemParameter", null, username, requestId, storeId,
            channelId, clientId, null, null);
    try {
      this.systemParameterConfigService.delete(storeId, username, variable);
      return new GdnBaseRestResponse(true);
    } catch (Exception e){
      log.error(LoggerStandard.getLogErrorTemplate(loggerModel, e), e);
      return new GdnBaseRestResponse(false);
    }
  }

  @RequestMapping(value = {SystemParameterConfigController.SYSTEM_PARAMETER_FIND_ONE}, method = {RequestMethod.GET},
      produces = {MediaType.APPLICATION_JSON_VALUE})
  @ResponseBody
  public GdnRestSingleResponse<SystemParameterConfigResponse> findOne(@RequestParam String storeId,
      @RequestParam String requestId, @RequestParam String channelId,
      @RequestParam String clientId, @RequestParam(required = false) String username, @RequestParam String variable) {
    LoggerAttributeModel loggerModel =
        new LoggerAttributeModel(this, "findOne", null, username, requestId, storeId,
            channelId, clientId, null, null);
    SystemParameterConfigResponse systemParameterResponse = new SystemParameterConfigResponse();
    try {
      SystemParameterConfig systemParameter =
          this.systemParameterConfigService.findValueByStoreIdAndVariable(storeId, variable);
      BeanUtils.copyProperties(systemParameter, systemParameterResponse);
    } catch (Exception e){
      log.error(LoggerStandard.getLogErrorTemplate(loggerModel, e), e);
    }
    return new GdnRestSingleResponse<>(systemParameterResponse, requestId);
  }

  @RequestMapping(value = {SystemParameterConfigController.SYSTEM_PARAMETER_ADD}, method = {RequestMethod.POST},
      consumes = {MediaType.APPLICATION_JSON_VALUE}, produces = {MediaType.APPLICATION_JSON_VALUE})
  @ResponseBody
  public GdnBaseRestResponse insertSystemParamter(@RequestParam String storeId, @RequestParam String requestId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam(required = false) String username,
      @RequestBody SystemParameterConfigRequest systemParameterConfigRequest) {
    LoggerAttributeModel loggerModel =
        new LoggerAttributeModel(this, "findOne", null, username, requestId, storeId,
            channelId, clientId, null, null);
    try {
      this.systemParameterConfigService.insert(storeId, username, systemParameterConfigRequest);
      return new GdnBaseRestResponse(true);
    } catch (Exception e){
      log.error(LoggerStandard.getLogErrorTemplate(loggerModel, e), e);
      return new GdnBaseRestResponse(false);
    }
  }

  @RequestMapping(value = {SystemParameterConfigController.SYSTEM_PARAMETER_UPDATE}, method = {RequestMethod.PUT},
      consumes = {MediaType.APPLICATION_JSON_VALUE}, produces = {MediaType.APPLICATION_JSON_VALUE})
  @ResponseBody
  public GdnBaseRestResponse updateSystemParamter(@RequestParam String storeId, @RequestParam String requestId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam(required = false) String username,
      @RequestBody SystemParameterConfigRequest systemParameterConfigRequest) {
    LoggerAttributeModel loggerModel =
        new LoggerAttributeModel(this, "findOne", null, username, requestId, storeId,
            channelId, clientId, null, null);
    try {
      this.systemParameterConfigService.update(storeId, username, systemParameterConfigRequest);
      return new GdnBaseRestResponse(true);
    } catch (Exception e){
      log.error(LoggerStandard.getLogErrorTemplate(loggerModel, e), e);
      return new GdnBaseRestResponse(false);
    }
  }
}
