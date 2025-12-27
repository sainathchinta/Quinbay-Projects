package com.gdn.partners.pbp.controller.product.reject;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.slf4j.MDC;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.bind.annotation.RestController;

import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.partner.pbp.annotations.AuditLog;
import com.gdn.partner.pbp.logger.standar.LoggerAspect;
import com.gdn.partner.pbp.logger.standar.LoggerAttributeModel;
import com.gdn.partner.pbp.logger.standar.LoggerParam;
import com.gdn.partner.pbp.logger.standar.LoggerStandard;
import com.gdn.partners.pbp.workflow.product.ProductWipAutoRejectService;

import io.swagger.v3.oas.annotations.tags.Tag;
import io.swagger.v3.oas.annotations.Operation;

@RestController
@RequestMapping(value = ProductWipAutoRejectControllerPath.BASE_PATH)
@Tag(name = "ProductAutoRejectController", description = "Product Wip Auto Reject Service API")
public class ProductWipAutoRejectController {

  private static final Logger LOGGER = LoggerFactory.getLogger(ProductWipAutoRejectController.class);

  @Autowired
  private ProductWipAutoRejectService productWipAutoRejectService;

  @AuditLog
  @RequestMapping(value = ProductWipAutoRejectControllerPath.AUTO_REJECT, method = RequestMethod.GET, produces = {
      MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "auto reject product wip in state need correction",
      description = "auto reject product wip in state need correction")
  @ResponseBody
  public GdnBaseRestResponse autoRejectProductWipNeedCorrection(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam String username) throws Exception {

    LoggerAttributeModel loggerAttribute =
        new LoggerAttributeModel(this, "autoRejectProductWipNeedCorrection", null, username, requestId, storeId,
            channelId, clientId, LoggerAspect.PRODUCT_DELETE, null, null);

    MDC.put(LoggerParam.GENERIC_LOGGER.getParam(), LoggerStandard.getGenericLogTemplate(loggerAttribute));
    LOGGER.debug(LoggerStandard.getLogInfoTemplate(loggerAttribute));

    productWipAutoRejectService.autoRejectProductWipNeedCorrectionExpired(storeId);
    return new GdnBaseRestResponse(null, null, true, requestId);
  }

}
