package com.gdn.mta.product.controller;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.slf4j.MDC;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.bind.annotation.RestController;

import com.gda.mta.product.dto.RecategorizationRequest;
import com.gdn.mta.product.util.GdnMandatoryRequestParameterUtil;
import com.gdn.mta.product.service.RecategorizationService;
import com.gdn.mta.product.util.ControllerUtils;
import com.gdn.mta.product.util.GdnRestSimpleResponse;
import com.gdn.mta.product.web.model.RecategorizationControllerPath;
import com.gdn.partner.pbp.logger.standar.LoggerAspect;
import com.gdn.partner.pbp.logger.standar.LoggerAttributeModel;
import com.gdn.partner.pbp.logger.standar.LoggerParam;
import com.gdn.partner.pbp.logger.standar.LoggerStandard;

import io.swagger.v3.oas.annotations.tags.Tag;
import io.swagger.v3.oas.annotations.Operation;

/**
 * Created by hardikbohra on 10/05/18.
 */

@RestController
@RequestMapping(value = RecategorizationControllerPath.BASE_PATH)
@Tag(name = "RecategorizationController", description = "Recategorization Service API")
public class RecategorizationController {

  private static final Logger LOGGER = LoggerFactory.getLogger(RecategorizationController.class);

  @Autowired
  private RecategorizationService recategorizationService;

  @RequestMapping(value = RecategorizationControllerPath.CREATE, method = RequestMethod.POST
      , produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "save recategorization", description = "save recategorization")
  @ResponseBody
  public GdnRestSimpleResponse<String> createRecategorization(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam String username, @RequestBody RecategorizationRequest request) throws Exception {

    LoggerAttributeModel loggerAttribute = new LoggerAttributeModel(this, "saveRecategorization", null,
        username, requestId, storeId, channelId, clientId, LoggerAspect.RECATEGORIZATION_CREATE,
        request.getName(), request.toString());

    MDC.put(LoggerParam.GENERIC_LOGGER.getParam(), LoggerStandard.getGenericLogTemplate(loggerAttribute));
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, username);
    MDC.put(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER, storeId);
    LOGGER.debug(LoggerStandard.getLogInfoTemplate(loggerAttribute));

    ControllerUtils.validateRecategorizationRequest(request);

    String recatId = recategorizationService.save(request);

    return new GdnRestSimpleResponse<String>(null, null, true, requestId, recatId);
  }
}
