package com.gdn.mta.product.controller;

import java.util.Objects;

import org.apache.commons.lang3.StringUtils;
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

import com.gda.mta.product.dto.ProductBusinessPartnerConfigRequest;
import com.gdn.common.base.GdnPreconditions;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.mta.product.util.GdnRestSimpleResponse;
import com.gdn.mta.product.web.model.ProductBusinessPartnerConfigControllerPath;
import com.gdn.mta.product.web.model.ProductBusinessPartnerConfigErrorMessage;
import com.gdn.partner.pbp.annotations.AuditLog;
import com.gdn.partner.pbp.logger.standar.LoggerAspect;
import com.gdn.partner.pbp.logger.standar.LoggerAttributeModel;
import com.gdn.partner.pbp.logger.standar.LoggerParam;
import com.gdn.partner.pbp.logger.standar.LoggerStandard;
import com.gdn.partners.pbp.service.bpconfig.ProductBusinessPartnerConfigService;

import io.swagger.v3.oas.annotations.tags.Tag;
import io.swagger.v3.oas.annotations.Operation;

/**
 * Created by Vishal on 18/05/18.
 */
@RestController
@RequestMapping(value = ProductBusinessPartnerConfigControllerPath.BASE_PATH)
@Tag(name = "ProductBusinessPartnerConfigController", description = "Product Business Partner Config API")
public class ProductBusinessPartnerConfigController {



  private static final Logger LOGGER =
      LoggerFactory.getLogger(ProductBusinessPartnerConfigController.class);


  @Autowired
  private ProductBusinessPartnerConfigService service;


  @AuditLog
  @RequestMapping(value = ProductBusinessPartnerConfigControllerPath.ENABLE_PRODUCT_MAIL_NOTIFY, method =
      RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "mail to merchant care option visibility true or false", description = "mail to"
      + " merchant care option visibility true or false")
  @ResponseBody
  public GdnRestSimpleResponse<Boolean> notifyMailVisibilityOptionForProductWip(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam(required = false) String username,
      @RequestParam String businessPartnerCode) {
    LoggerAttributeModel loggerAttribute =
        new LoggerAttributeModel(this, "notifyMailVisibilityOptionForProductWip",
            businessPartnerCode, username, requestId, storeId, channelId, clientId,
            LoggerAspect.BUSINESS_PARTNER_CONFIG, null, null);
    MDC.put(LoggerParam.GENERIC_LOGGER.getParam(),
        LoggerStandard.getGenericLogTemplate(loggerAttribute));
    ProductBusinessPartnerConfigController.LOGGER
        .debug(LoggerStandard.getLogInfoTemplate(loggerAttribute));
    GdnPreconditions.checkArgument(StringUtils.isNotEmpty(businessPartnerCode),
        ProductBusinessPartnerConfigErrorMessage.BUSINESS_PARTNER_CODE_MUST_NOT_BLANK);
    boolean isNotifyMailVisible =
        service.notifyMailVisibilityOptionForProductWip(storeId, businessPartnerCode);
    return new GdnRestSimpleResponse<Boolean>(StringUtils.EMPTY, StringUtils.EMPTY, Boolean.TRUE, requestId,
        isNotifyMailVisible);
  }

  @AuditLog
  @RequestMapping(value = ProductBusinessPartnerConfigControllerPath.SAVE, method =
      RequestMethod.POST, produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "save mail to merchant care detail entry",
      description = "save mail to merchant care detail entry")
  @ResponseBody
  public GdnBaseRestResponse save(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam(required = false) String username,
      @RequestBody ProductBusinessPartnerConfigRequest request) {
    LoggerAttributeModel loggerAttribute =
        new LoggerAttributeModel(this, "save",
            request.getBpCode(), username, requestId, storeId, channelId, clientId,
            LoggerAspect.BUSINESS_PARTNER_CONFIG, null, null);
    MDC.put(LoggerParam.GENERIC_LOGGER.getParam(),
        LoggerStandard.getGenericLogTemplate(loggerAttribute));
    ProductBusinessPartnerConfigController.LOGGER
        .debug(LoggerStandard.getLogInfoTemplate(loggerAttribute));
    GdnPreconditions.checkArgument(StringUtils.isNotEmpty(request.getBpCode()),
        ProductBusinessPartnerConfigErrorMessage.BUSINESS_PARTNER_CODE_MUST_NOT_BLANK);
    GdnPreconditions.checkArgument(Objects.nonNull(request.getProductToActivateNotifyMailDate()),
        ProductBusinessPartnerConfigErrorMessage.MAIL_NOTIFY_DATE_MUST_NOT_BLANK);
    service.save(storeId, username, request);
    return new GdnBaseRestResponse(StringUtils.EMPTY, StringUtils.EMPTY, Boolean.TRUE, requestId);
  }
}
