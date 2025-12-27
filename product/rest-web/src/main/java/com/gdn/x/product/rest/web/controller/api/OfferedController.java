package com.gdn.x.product.rest.web.controller.api;

import com.gdn.common.web.param.MandatoryRequestParam;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.x.product.model.vo.OfferedSummaryVo;
import com.gdn.x.product.service.util.ModelConverter;
import com.gdn.x.product.rest.web.model.ProductApiPath;
import com.gdn.x.product.rest.web.model.enums.ProductErrorCodesEnum;
import com.gdn.x.product.rest.web.model.request.OfferedSummaryRequest;
import com.gdn.x.product.rest.web.model.response.OfferedComboSummaryResponse;
import com.gdn.x.product.service.api.OfferedService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;



@Controller
@RequestMapping(value = ProductApiPath.OFFERED)
@Tag(name = "Offered Summary Controller", description = "Offered Summary Service API")
public class OfferedController {

  private static final Logger LOG = LoggerFactory.getLogger(OfferedController.class);

  @Autowired
  private OfferedService offeredService;

  @Autowired
  private ModelConverter modelConverter;

  @Deprecated
  @RequestMapping(value = {ProductApiPath.OFFERED_SUMMARY}, method = {
      RequestMethod.POST}, produces = {MediaType.APPLICATION_JSON_VALUE}, consumes = {
      MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "grouping all attributes by item code or pristine id and item sku")
  @ResponseBody
  public GdnRestSingleResponse<OfferedComboSummaryResponse> getOfferedSummary(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam(required = false) String username, @RequestBody OfferedSummaryRequest request)
      throws Exception {

    MandatoryRequestParam param = MandatoryRequestParam.generateMandatoryRequestParam(storeId,
        channelId, clientId, requestId);
    LOG.info("Get offered summary with request {}", request);
    OfferedComboSummaryResponse offeredComboSummaryResponse = new OfferedComboSummaryResponse();

    try {
      OfferedSummaryVo offeredSummaryVo = offeredService
          .getOfferedSummary(param, request.getPristineId(), request.getItemCode(),
              request.getItemSku(), request.getDefaultSku());
      offeredComboSummaryResponse =
          modelConverter.convertToResponse(offeredSummaryVo, OfferedComboSummaryResponse.class);
      return new GdnRestSingleResponse<>(offeredComboSummaryResponse, requestId);
    } catch (Exception e) {
      LOG.error("Failed to get offered summary with request {}", request, e);
      return new GdnRestSingleResponse<>(ProductErrorCodesEnum.INTERNAL_SERVER.getMessage(),
          ProductErrorCodesEnum.INTERNAL_SERVER.getCode(), false, offeredComboSummaryResponse,
          requestId);
    }
  }

  @Deprecated
  @RequestMapping(value = ProductApiPath.OFFERED_COMBO_SUMMARY, method = RequestMethod.POST,
      produces = MediaType.APPLICATION_JSON_VALUE, consumes = {
      MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "grouping all attributes by pristine id or item code or item sku")
  @ResponseBody
  public GdnRestSingleResponse<OfferedComboSummaryResponse> getOfferedComboSummary(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam(required = false) String username,
      @RequestBody OfferedSummaryRequest request) throws Exception {
    MandatoryRequestParam param = MandatoryRequestParam.generateMandatoryRequestParam(storeId,
        channelId, clientId, requestId);
    LOG.info("Get offered combo summary with request : {}", request);

    OfferedComboSummaryResponse offeredComboSummaryResponse = new OfferedComboSummaryResponse();
    try {
      OfferedSummaryVo offeredSummaryVo =
          offeredService.getOfferedComboSummary(param, request.getPristineId(),
              request.getItemCode(), request.getItemSku(), request.getDefaultSku());

      offeredComboSummaryResponse =
          modelConverter.convertToResponse(offeredSummaryVo, OfferedComboSummaryResponse.class);
      return new GdnRestSingleResponse<>(offeredComboSummaryResponse, requestId);
    } catch (Exception e) {
      LOG.error("Failed to get offered combo summary with request : {}", request, e);
      return new GdnRestSingleResponse<>(ProductErrorCodesEnum.INTERNAL_SERVER.getMessage(),
          ProductErrorCodesEnum.INTERNAL_SERVER.getCode(), false, offeredComboSummaryResponse,
          requestId);
    }
  }
}
