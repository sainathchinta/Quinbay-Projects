package com.gdn.partners.pcu.internal.web.controller;

import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.partners.core.web.dto.SingleBaseResponse;
import com.gdn.partners.pcu.internal.client.helper.ClientParameterHelper;
import com.gdn.partners.pcu.internal.client.model.request.SystemParameterRequest;
import com.gdn.partners.pcu.internal.client.model.response.ProductSystemParameterResponse;
import com.gdn.partners.pcu.internal.client.model.response.SystemParameterResponse;
import com.gdn.partners.pcu.internal.model.SystemParameterApiPath;
import com.gdn.partners.pcu.internal.service.SystemParameterService;
import io.swagger.annotations.Api;
import io.swagger.v3.oas.annotations.Operation;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import java.util.List;

@Slf4j
@RestController
@RequiredArgsConstructor
@Api("System Parameter Api")
@RequestMapping(SystemParameterApiPath.BASE_PATH)
public class SystemParameterController {

  @Autowired
  private ClientParameterHelper clientParameterHelper;

  @Autowired
  private SystemParameterService systemParameterService;

  @Operation(summary = "Update system parameter for pbp")
  @PutMapping(value = SystemParameterApiPath.UPDATE_SYSTEM_PARAMETER, consumes =
      MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
  public GdnBaseRestResponse updateSystemParameter(
      @RequestBody List<SystemParameterRequest> productSystemParameterRequest) {
    log.info("Update system parameter for pbp {}", productSystemParameterRequest);
    systemParameterService.updateSystemParameter(clientParameterHelper.getStoreId(),
        clientParameterHelper.getChannelId(), clientParameterHelper.getClientId(),
        clientParameterHelper.getRequestId(), clientParameterHelper.getUsername(),
        productSystemParameterRequest);
    return new GdnBaseRestResponse(true);
  }

  @Operation(summary = "Show the UI-based configurations with values")
  @GetMapping(value = SystemParameterApiPath.GET_ALL_SYSTEM_PARAMETER_WITH_SHOW_ON_UI, produces =
      MediaType.APPLICATION_JSON_VALUE)
  public GdnRestListResponse<ProductSystemParameterResponse> showUIBasedConfiguration(
      @RequestParam String storeId) {
    return systemParameterService.fetchSystemParameterShowOnUI(storeId,
        clientParameterHelper.getChannelId(), clientParameterHelper.getClientId(),
        clientParameterHelper.getRequestId());
  }

  @Operation(summary = "Fetch System parameter switch")
  @GetMapping(value = SystemParameterApiPath.FETCH_SYSTEM_PARAMETER)
  public SingleBaseResponse<SystemParameterResponse> getSystemParameterSwitches() {
    log.info("Fetching internal switch values");
    return new SingleBaseResponse<>(null, null, true, this.clientParameterHelper.getRequestId(),
      this.systemParameterService.getSystemParameterSwitches(clientParameterHelper.getStoreId(),
        clientParameterHelper.getRequestId()));
  }
}
