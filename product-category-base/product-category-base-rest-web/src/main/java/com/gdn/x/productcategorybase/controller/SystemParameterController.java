package com.gdn.x.productcategorybase.controller;

import com.gdn.common.util.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;

import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.x.productcategorybase.SystemParameterControllerPath;
import com.gdn.x.productcategorybase.dto.request.SystemParameterRequest;
import com.gdn.x.productcategorybase.dto.response.SystemParameterResponse;
import com.gdn.x.productcategorybase.entity.SystemParameter;
import com.gdn.x.productcategorybase.service.SystemParameterService;

import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.extern.slf4j.Slf4j;
import org.springframework.web.bind.annotation.RestController;

@Slf4j
@RestController
@RequestMapping(value = SystemParameterControllerPath.BASE_PATH)
@Tag(name = "SystemParameterController", description = "Controller for system parameter")
public class SystemParameterController {

  @Autowired
  private SystemParameterService systemParameterService;

  @RequestMapping(value = SystemParameterControllerPath.SYSTEM_PARAMETER_INSERT, method =
      RequestMethod.POST, consumes = MediaType.APPLICATION_JSON_VALUE, produces =
      MediaType.APPLICATION_JSON_VALUE)
  public GdnBaseRestResponse insertSystemParameter(@RequestParam String storeId,
      @RequestParam String requestId,
      @RequestParam String channelId, @RequestParam String clientId,
      @RequestBody SystemParameterRequest systemParameterRequest) throws Exception {
    log.info("insert new system parameter with request : {}", systemParameterRequest);
    SystemParameter systemParameter = new SystemParameter();
    BeanUtils.copyProperties(systemParameterRequest, systemParameter);
    systemParameter.setStoreId(storeId);
    this.systemParameterService.insert(systemParameter);
    return new GdnBaseRestResponse(true);
  }

  @RequestMapping(value = SystemParameterControllerPath.SYSTEM_PARAMETER_UPDATE, method =
      RequestMethod.PUT, consumes = MediaType.APPLICATION_JSON_VALUE, produces =
      MediaType.APPLICATION_JSON_VALUE)
  public GdnBaseRestResponse updateSystemParameter(@RequestParam String storeId,
      @RequestParam String requestId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestBody SystemParameterRequest systemParameterRequest) throws Exception {
    log.info("update system parameter for variable : {}", systemParameterRequest.getVariable());
    SystemParameter systemParameter = new SystemParameter();
    BeanUtils.copyProperties(systemParameterRequest, systemParameter);
    systemParameter.setStoreId(storeId);
    this.systemParameterService.update(systemParameter);
    return new GdnBaseRestResponse(true);
  }

  @RequestMapping(value = SystemParameterControllerPath.SYSTEM_PARAMETER_DELETE, method =
      RequestMethod.DELETE, produces = MediaType.APPLICATION_JSON_VALUE)
  public GdnBaseRestResponse deleteSystemParameter(@RequestParam String storeId,
      @RequestParam String requestId, @RequestParam String channelId, @RequestParam String clientId,
      @PathVariable String variable) throws Exception {
    log.info("delete system parameter for variable : {}", variable);
    this.systemParameterService.delete(storeId, variable);
    return new GdnBaseRestResponse(true);
  }

  @RequestMapping(value = SystemParameterControllerPath.SYSTEM_PARAMETER_FIND, method =
      RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
  public GdnRestSingleResponse<SystemParameterResponse> findSystemParameter(
      @RequestParam String storeId, @RequestParam String requestId, @RequestParam String channelId,
      @RequestParam String clientId, @RequestParam String variable) throws Exception {
    log.info("get the system parameter for variable : {}", variable);
    SystemParameterResponse systemParameterResponse = new SystemParameterResponse();
    SystemParameter response = this.systemParameterService.findByStoreIdAndVariable(storeId, variable);
    BeanUtils.copyProperties(response, systemParameterResponse);
    return new GdnRestSingleResponse<>(systemParameterResponse, requestId);
  }
}
