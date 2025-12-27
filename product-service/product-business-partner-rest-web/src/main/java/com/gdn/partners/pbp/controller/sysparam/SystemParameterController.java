package com.gdn.partners.pbp.controller.sysparam;

import java.util.List;

import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.bind.annotation.RestController;

import com.gdn.common.base.GdnPreconditions;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.partners.pbp.dto.sysparam.SystemParameterRequest;
import com.gdn.partners.pbp.dto.sysparam.SystemParameterResponse;
import com.gdn.partners.pbp.service.sysparam.SystemParameterService;
import com.gdn.partners.pbp.web.model.SystemParameterControllerPath;

import io.swagger.v3.oas.annotations.tags.Tag;
import io.swagger.v3.oas.annotations.Operation;

@RestController
@RequestMapping(value = SystemParameterControllerPath.BASE_PATH)
@Tag(name = "System Parameter Controller",
    description = "API for configuring PBP System Parameter")
public class SystemParameterController {

  @Autowired
  @Qualifier("systemParameterService")
  private SystemParameterService sysparamService;

  /**
   * Actually this API didn't need X framework mandatory parameter, but since it's couldn't be
   * overriden, then we just specify default value
   */
  private static final String DEFAULT_STORE_ID = "10001";
  private static final String DEFAULT_CHANNEL_ID = "API";
  private static final String DEFAULT_CLIENT_ID = "API";

  @RequestMapping(value = SystemParameterControllerPath.RELOAD, method = RequestMethod.GET
      , produces = MediaType.APPLICATION_JSON_VALUE)
  @ResponseBody
  @Operation(summary = "Reload all system parameters",
      description = "use fullReload=true to reload from sysparam.properties, otherwise reload from redis")
  public GdnBaseRestResponse reloadParameters(
      @RequestParam(defaultValue = DEFAULT_STORE_ID) String storeId, @RequestParam(
          defaultValue = DEFAULT_CHANNEL_ID) String channelId, @RequestParam(
          defaultValue = DEFAULT_CLIENT_ID) String clientId, @RequestParam String username,
      @RequestParam String requestId, @RequestParam(defaultValue = "false") Boolean fullReload)
      throws Exception {
    sysparamService.reload(fullReload);
    return new GdnBaseRestResponse(requestId);
  }

  @RequestMapping(value = "", method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
  @ResponseBody
  @Operation(summary = "Get system parameter by key",
      description = "See the system parameter key from sysparam.properties file")
  public GdnRestSingleResponse<SystemParameterResponse> getParameter(@RequestParam(
      defaultValue = DEFAULT_STORE_ID) String storeId, @RequestParam(
      defaultValue = DEFAULT_CHANNEL_ID) String channelId, @RequestParam(
      defaultValue = DEFAULT_CLIENT_ID) String clientId, @RequestParam String username,
      @RequestParam String requestId, @RequestParam String key) {
    SystemParameterResponse response = new SystemParameterResponse();
    Object value = sysparamService.getParameter(key);
    response.setValue(value);
    return new GdnRestSingleResponse<>(response, requestId);
  }

  @RequestMapping(value = "", method = RequestMethod.POST, produces = MediaType.APPLICATION_JSON_VALUE)
  @ResponseBody
  @Operation(summary = "Set system parameter value",
      description = "See the system parameter key from sysparam.properties file")
  public GdnBaseRestResponse setParameter(
      @RequestParam(defaultValue = DEFAULT_STORE_ID) String storeId, @RequestParam(
          defaultValue = DEFAULT_CHANNEL_ID) String channelId, @RequestParam(
          defaultValue = DEFAULT_CLIENT_ID) String clientId, @RequestParam String username,
      @RequestParam String requestId, @RequestBody SystemParameterRequest request) {
    GdnPreconditions.checkArgument(request != null, "Request body cannot be empty");
    GdnPreconditions.checkArgument(StringUtils.isNotEmpty(request.getKey()),
        "System parameter key cannot be empty");
    sysparamService.setParameter(request.getKey(), request.getValue());
    return new GdnBaseRestResponse(requestId);
  }

  @RequestMapping(value = "/keys", method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
  @ResponseBody
  @Operation(summary = "Get available system parameter keys",
      description = "Get available system parameter keys")
  public List<String> getParameterKeys(
      @RequestParam(defaultValue = DEFAULT_STORE_ID) String storeId, @RequestParam(
          defaultValue = DEFAULT_CHANNEL_ID) String channelId, @RequestParam(
          defaultValue = DEFAULT_CLIENT_ID) String clientId, @RequestParam String username,
      @RequestParam String requestId) {
    return sysparamService.keys();
  }
}
