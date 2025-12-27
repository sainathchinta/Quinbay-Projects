package com.gdn.x.mta.distributiontask.controller;

import org.apache.commons.lang3.StringUtils;
import org.slf4j.MDC;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;

import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.x.mta.distributiontask.util.GdnMandatoryRequestParameterUtil;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.x.mta.distributiontask.rest.model.ReportProductControllerPath;
import com.gdn.x.mta.distributiontask.rest.model.request.ReportProductRequest;
import com.gdn.x.mta.distributiontask.service.api.ReportProductService;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.extern.slf4j.Slf4j;

@Controller
@Slf4j
@RequestMapping(value = ReportProductControllerPath.BASE_PATH)
@Tag(name = "ReportProductController", description = "Report any issue with the product")
public class ReportProductController {

  @Autowired
  private ReportProductService reportProductService;

  @RequestMapping(value = ReportProductControllerPath.CREATE_REPORT_PRODUCT,
      method = RequestMethod.POST, produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Report any issue with the product", description = "Report any issue with the product")
  @ResponseBody
  public GdnBaseRestResponse createReportProduct(
      @RequestParam String storeId, @RequestParam String channelId,
      @RequestParam String clientId, @RequestParam String requestId, @RequestParam String username,
      @RequestBody ReportProductRequest reportProductRequest) throws Exception {
    MDC.put(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER, storeId);
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, username);
    MDC.put(GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER, requestId);
    try {
      reportProductService.addReportProduct(reportProductRequest);
      return new GdnBaseRestResponse(StringUtils.EMPTY, StringUtils.EMPTY, true, requestId);
    } catch (ApplicationRuntimeException e) {
      log.error("Exception caught while saving product report", e);
      return new GdnBaseRestResponse(e.getErrorMessage(), e.getErrorCodes().getCode(), false, requestId);
    }
    catch (Exception e) {
      log.error("Exception caught while saving product report", e);
      return new GdnBaseRestResponse(ErrorCategory.UNSPECIFIED.getMessage(), ErrorCategory.UNSPECIFIED.getCode(), false,
          requestId);
    }
  }
}
