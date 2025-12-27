package com.gdn.x.product.rest.web.controller.api;

import java.util.ArrayList;
import java.util.List;

import org.apache.commons.beanutils.PropertyUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.common.web.wrapper.response.PageMetaData;
import com.gdn.x.product.model.entity.SystemParameter;
import com.gdn.x.product.rest.web.model.ApiPath;
import com.gdn.x.product.rest.web.model.SystemParameterRequest;
import com.gdn.x.product.rest.web.model.SystemParameterResponse;
import com.gdn.x.product.service.api.SystemParameterService;
import io.swagger.v3.oas.annotations.tags.Tag;

@RestController
@RequestMapping(value = ApiPath.SYSTEM_PARAMETER_PATH)
@Tag(name = "SystemParameter", description = "SystemParameter Service API")
public class SystemParameterController {

  @Autowired
  private SystemParameterService systemParameterService;

  @RequestMapping(value = {ApiPath.SYSTEM_PARAMETER_DELETE}, method = {RequestMethod.DELETE},
      produces = {MediaType.APPLICATION_JSON_VALUE})
  public GdnBaseRestResponse deleteSystemParameter(@RequestParam String storeId,
      @RequestParam String requestId, @RequestParam String channelId,
      @RequestParam String clientId, @RequestParam String variable)
      throws ReflectiveOperationException {
    this.systemParameterService.delete(storeId, variable);
    return new GdnBaseRestResponse(true);
  }

  @RequestMapping(value = {ApiPath.SYSTEM_PARAMETER_FIND_ALL}, method = {RequestMethod.GET},
      produces = {MediaType.APPLICATION_JSON_VALUE})
  public GdnRestListResponse<SystemParameterResponse> findAll(@RequestParam String storeId,
      @RequestParam String requestId, @RequestParam String channelId, @RequestParam String clientId)
      throws ReflectiveOperationException {

    List<SystemParameter> systemParameters = this.systemParameterService.findAll(storeId);
    List<SystemParameterResponse> systemParameterResponses = new ArrayList<>();

    for (SystemParameter systemParameter : systemParameters) {
      SystemParameterResponse systemParameterResponse = new SystemParameterResponse();
      PropertyUtils.copyProperties(systemParameterResponse, systemParameter);
      systemParameterResponses.add(systemParameterResponse);
    }

    int size = systemParameterResponses.size();
    return new GdnRestListResponse<SystemParameterResponse>(systemParameterResponses,
        new PageMetaData(size, 0, size), requestId);
  }

  @RequestMapping(value = {ApiPath.SYSTEM_PARAMETER_FIND_ONE}, method = {RequestMethod.GET},
      produces = {MediaType.APPLICATION_JSON_VALUE})
  public GdnRestSingleResponse<SystemParameterResponse> findOne(@RequestParam String storeId,
      @RequestParam String requestId, @RequestParam String channelId,
      @RequestParam String clientId, @RequestParam String variable)
      throws ReflectiveOperationException {
    SystemParameter systemParameter =
        this.systemParameterService.findValueByStoreIdAndVariable(storeId, variable);
    SystemParameterResponse systemParameterResponse = new SystemParameterResponse();
    PropertyUtils.copyProperties(systemParameterResponse, systemParameter);

    return new GdnRestSingleResponse<SystemParameterResponse>(systemParameterResponse, requestId);
  }

  @RequestMapping(value = {ApiPath.SYSTEM_PARAMETER_INSERT}, method = {RequestMethod.PUT},
      consumes = {MediaType.APPLICATION_JSON_VALUE}, produces = {MediaType.APPLICATION_JSON_VALUE})
  public GdnBaseRestResponse insertSystemParamter(@RequestParam String storeId,
      @RequestParam String requestId, @RequestParam String channelId,
      @RequestParam String clientId, @RequestBody SystemParameterRequest systemParameterRequest)
      throws Exception {
    SystemParameter systemParameter = new SystemParameter();
    PropertyUtils.copyProperties(systemParameter, systemParameterRequest);
    systemParameter.setStoreId(storeId);
    this.systemParameterService.insert(systemParameter);
    return new GdnBaseRestResponse(true);
  }

  @RequestMapping(value = {ApiPath.SYSTEM_PARAMETER_UPDATE}, method = {RequestMethod.POST},
      consumes = {MediaType.APPLICATION_JSON_VALUE}, produces = {MediaType.APPLICATION_JSON_VALUE})
  public GdnBaseRestResponse updateSystemParamter(@RequestParam String storeId,
      @RequestParam String requestId, @RequestParam String channelId,
      @RequestParam String clientId, @RequestBody SystemParameterRequest systemParameterRequest)
      throws Exception {
    SystemParameter systemParameter = new SystemParameter();
    PropertyUtils.copyProperties(systemParameter, systemParameterRequest);
    systemParameter.setStoreId(storeId);
    this.systemParameterService.update(systemParameter);
    return new GdnBaseRestResponse(true);
  }
}
