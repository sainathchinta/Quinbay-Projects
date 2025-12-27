package com.gdn.x.mta.distributiontask.controller;


import com.gdn.x.mta.distributiontask.response.SystemParameterResponse;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;

import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.x.mta.distributiontask.model.SystemParameterConfig;
import com.gdn.x.mta.distributiontask.request.SystemParameterConfigRequest;
import com.gdn.x.mta.distributiontask.response.SystemParameterConfigResponse;
import com.gdn.x.mta.distributiontask.service.api.SystemParameterConfigService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.extern.slf4j.Slf4j;
import org.springframework.web.bind.annotation.RestController;

@Slf4j
@RestController
@RequestMapping(value = SystemParameterConfigController.BASE_PATH)
@Tag(name = "SystemParameterConfigController", description = "SystemParameterConfig API")
public class SystemParameterConfigController {

  public static final String BASE_PATH = "/api/system-parameters";
  public static final String SYSTEM_PARAMETER_DELETE = "/delete";
  public static final String SYSTEM_PARAMETER_FIND_ONE = "/find-one";
  public static final String SYSTEM_PARAMETER_ADD = "/add";
  public static final String SYSTEM_PARAMETER_UPDATE = "/update";
  public static final String FETCH_SYSTEM_PARAMETERS = "/fetch-system-parameter";

  @Autowired
  private SystemParameterConfigService systemParameterConfigService;

  @DeleteMapping(value = {SystemParameterConfigController.SYSTEM_PARAMETER_DELETE}, produces = {
    MediaType.APPLICATION_JSON_VALUE})
  @Operation
  public GdnBaseRestResponse deleteSystemParameter(@RequestParam String storeId,
      @RequestParam String requestId, @RequestParam String channelId,
      @RequestParam String clientId, @RequestParam(required = false) String username, @RequestParam String variable) {
    try {
      this.systemParameterConfigService.delete(storeId, username, variable);
      return new GdnBaseRestResponse(true);
    } catch (Exception e){
      log.error("Exception caught while deleting system parameter ", e);
      return new GdnBaseRestResponse(false);
    }
  }

  @GetMapping(value = {SystemParameterConfigController.SYSTEM_PARAMETER_FIND_ONE}, produces = {
    MediaType.APPLICATION_JSON_VALUE})
  @Operation
  public GdnRestSingleResponse<SystemParameterConfigResponse> findOne(@RequestParam String storeId,
      @RequestParam String requestId, @RequestParam String channelId,
      @RequestParam String clientId, @RequestParam(required = false) String username, @RequestParam String variable) {
    SystemParameterConfigResponse systemParameterResponse = new SystemParameterConfigResponse();
    try {
      SystemParameterConfig systemParameter =
          this.systemParameterConfigService.findValueByStoreIdAndVariable(storeId, variable);
      BeanUtils.copyProperties(systemParameter, systemParameterResponse);
    } catch (Exception e){
      log.error("Exception caught while finding system parameter ", e);
    }
    return new GdnRestSingleResponse<>(systemParameterResponse, requestId);
  }

  @PostMapping(value = {SystemParameterConfigController.SYSTEM_PARAMETER_ADD}, consumes = {
    MediaType.APPLICATION_JSON_VALUE}, produces = {MediaType.APPLICATION_JSON_VALUE})
  @Operation
  public GdnBaseRestResponse insertSystemParamter(@RequestParam String storeId, @RequestParam String requestId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam(required = false) String username,
      @RequestBody SystemParameterConfigRequest systemParameterConfigRequest) {
    try {
      this.systemParameterConfigService.insert(storeId, username, systemParameterConfigRequest);
      return new GdnBaseRestResponse(true);
    } catch (Exception e){
      log.error("Exception caught while inserting system parameter ", e);
      return new GdnBaseRestResponse(false);
    }
  }

  @PutMapping(value = {SystemParameterConfigController.SYSTEM_PARAMETER_UPDATE}, consumes = {
    MediaType.APPLICATION_JSON_VALUE}, produces = {MediaType.APPLICATION_JSON_VALUE})
  @Operation
  public GdnBaseRestResponse updateSystemParamter(@RequestParam String storeId, @RequestParam String requestId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam(required = false) String username,
      @RequestBody SystemParameterConfigRequest systemParameterConfigRequest) {
    try {
      this.systemParameterConfigService.update(storeId, username, systemParameterConfigRequest);
      return new GdnBaseRestResponse(true);
    } catch (Exception e){
      log.error("Exception caught while updating system parameter ", e);
      return new GdnBaseRestResponse(false);
    }
  }

  @GetMapping(value = SystemParameterConfigController.FETCH_SYSTEM_PARAMETERS,
    produces = MediaType.APPLICATION_JSON_VALUE)
  public GdnRestSingleResponse<SystemParameterResponse> findSystemParameterSwitchValues(
    @RequestParam String storeId, @RequestParam String requestId) {
    log.info("Fetching flag values for various switches from system parameter database");
    return new GdnRestSingleResponse<>(
      new SystemParameterResponse(this.systemParameterConfigService.findSwitchValues(storeId)),
      requestId);
  }
}