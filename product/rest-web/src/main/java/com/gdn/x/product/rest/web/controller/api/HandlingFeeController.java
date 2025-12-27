package com.gdn.x.product.rest.web.controller.api;

import java.util.List;

import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;

import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.x.product.model.entity.SystemParameter;
import com.gdn.x.product.model.vo.HandlingFeeResponse;
import com.gdn.x.product.service.util.ModelConverter;
import com.gdn.x.product.rest.web.model.ApiPath;
import com.gdn.x.product.rest.web.model.SystemParameterRequest;
import com.gdn.x.product.rest.web.model.SystemParameterResponse;
import com.gdn.x.product.rest.web.model.request.HandlingFeeRequestRestWeb;
import com.gdn.x.product.rest.web.model.response.HandlingFeeResponseRestWeb;
import com.gdn.x.product.service.api.HandlingFeeService;
import org.springframework.web.bind.annotation.RestController;


@RestController
@Deprecated
@Tag(name = "HandlingFee", description = "HandlingFee Service API")
@RequestMapping(value = ApiPath.HANDLING_FEE_PATH)
public class HandlingFeeController {

  @Autowired
  private HandlingFeeService handlingFeeService;

  @Autowired
  private ModelConverter modelConverter;

  @RequestMapping(value = {ApiPath.HANDLING_FEE_CALCULATE_HANDLING_FEE},
      method = {RequestMethod.POST}, produces = {MediaType.APPLICATION_JSON_VALUE})
  public GdnRestSingleResponse<HandlingFeeResponseRestWeb> calculateHandlingFee(
      @RequestParam String storeId, @RequestParam String requestId, @RequestParam String channelId,
      @RequestParam String clientId, @RequestParam(required = false) String username,
      @RequestBody List<HandlingFeeRequestRestWeb> handlingFeeRequestRestWebList)
          throws ReflectiveOperationException {

    HandlingFeeResponse handlingFeeResponse =
        this.handlingFeeService.calculateHandlingFee(storeId,
            this.modelConverter.convertToHandlingFeeRequestList(handlingFeeRequestRestWebList));

    return new GdnRestSingleResponse<HandlingFeeResponseRestWeb>(
        this.modelConverter.convertToHandlingFeeResponseRestWeb(handlingFeeResponse), requestId);
  }

  @RequestMapping(value = {ApiPath.HANDLING_FEE_GET_SETTING_OF_HANDLING_FEE},
      method = {RequestMethod.POST}, produces = {MediaType.APPLICATION_JSON_VALUE})
  public GdnRestSingleResponse<SystemParameterResponse> getAllSettingOfHandlingFee(
      @RequestParam String storeId, @RequestParam String requestId, @RequestParam String channelId,
      @RequestParam String clientId, @RequestParam(required = false) String username)
          throws ReflectiveOperationException {

    SystemParameter systemParameter = this.handlingFeeService.getAllSettingOfHandlingFee(storeId);

    return new GdnRestSingleResponse<SystemParameterResponse>(
        this.modelConverter.convertToSystemParameterResponse(systemParameter), requestId);
  }

  @RequestMapping(value = {ApiPath.HANDLING_FEE_UPDATE_SETTING_OF_HANDLING_FEE},
      method = {RequestMethod.POST}, produces = {MediaType.APPLICATION_JSON_VALUE})
  public GdnBaseRestResponse updateAllSettingOfHandlingFee(@RequestParam String storeId,
      @RequestParam String requestId, @RequestParam String channelId,
      @RequestParam String clientId, @RequestParam(required = false) String username,
      @RequestBody SystemParameterRequest systemParameterRequest)
          throws ReflectiveOperationException {

    SystemParameter systemParameter = new SystemParameter();
    systemParameter.setStoreId(storeId);
    this.modelConverter.convertToSystemParameter(systemParameterRequest, systemParameter);

    this.handlingFeeService.updateAllSettingOfHandlingFee(systemParameter);

    return new GdnBaseRestResponse(true);
  }
}
