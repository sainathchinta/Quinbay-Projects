package com.gdn.mta.product.controller;

import com.gdn.mta.product.service.ProductStockAlertWrapper;
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

import com.gda.mta.product.dto.ProductStockAlertResponse;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.mta.product.service.ProductLevel3Service;
import com.gdn.mta.product.service.ProductStockAlertService;
import com.gdn.mta.product.web.model.ProductStockAlertControllerPath;
import com.gdn.partner.pbp.annotations.AuditLog;
import com.gdn.partner.pbp.logger.standar.LoggerAspect;
import com.gdn.partner.pbp.logger.standar.LoggerAttributeModel;
import com.gdn.partner.pbp.logger.standar.LoggerParam;
import com.gdn.partner.pbp.logger.standar.LoggerStandard;

import io.swagger.v3.oas.annotations.tags.Tag;
import io.swagger.v3.oas.annotations.Operation;

@RestController
@RequestMapping(value = ProductStockAlertControllerPath.BASE_PATH)
@Tag(name = "ProductStockAlertController", description = "Product Stock Alert Service API")
public class ProductStockAlertController {

  private static final Logger LOGGER = LoggerFactory.getLogger(ProductStockAlertController.class);

  @Autowired
  private ProductLevel3Service productLevel3Service;

  @Autowired
  private ProductStockAlertService productStockAlertService;

  @Autowired
  private ProductStockAlertWrapper productStockAlertWrapper;

  public ProductLevel3Service getProductLevel3Service() {
    return this.productLevel3Service;
  }

  public void setProductLevel3Service(ProductLevel3Service productLevel3Service) {
    this.productLevel3Service = productLevel3Service;
  }

  @AuditLog
  @RequestMapping(value = ProductStockAlertControllerPath.UPDATE_ITEM_STOCK_ALERT_VIEW_CONFIG,
      method = RequestMethod.GET, produces = {MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "update item view configuration", description = "update item view configuration")
  @ResponseBody
  public GdnBaseRestResponse updateItemStockAlertViewConfig(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam(required = false) String username) {
    
    LoggerAttributeModel loggerAttribute = new LoggerAttributeModel(this, "updateItemStockAlertViewConfig", null, 
        username, requestId, storeId, channelId, clientId, LoggerAspect.PRODUCT_LV3_UPDATE, null, null);
    
    MDC.put(LoggerParam.GENERIC_LOGGER.getParam(), LoggerStandard.getGenericLogTemplate(loggerAttribute));
    LOGGER.debug(LoggerStandard.getLogInfoTemplate(loggerAttribute));
    
    this.productStockAlertService.autoBatchUpdateItemViewConfig();
    return new GdnBaseRestResponse(null, null, true, requestId);
  }

  @AuditLog
  @RequestMapping(value = ProductStockAlertControllerPath.EMAIL_ITEM_STOCK_ALERT, method = RequestMethod.GET, produces = {
      MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "send mail for item with stock alert",
      description = "send mail for item with stock alert")
  @ResponseBody
	public GdnBaseRestResponse sendMailAndNotificationItemStockAlert(
			@RequestParam String storeId, @RequestParam String channelId,
			@RequestParam String clientId, @RequestParam String requestId,
			@RequestParam(required = false) String username) {
    
    LoggerAttributeModel loggerAttribute = new LoggerAttributeModel(this, "sendMailAndNotificationItemStockAlert", null, 
        username, requestId, storeId, channelId, clientId, LoggerAspect.PRODUCT_LV3_SEND_EMAIL, null, null);
    
    MDC.put(LoggerParam.GENERIC_LOGGER.getParam(), LoggerStandard.getGenericLogTemplate(loggerAttribute));
    LOGGER.debug(LoggerStandard.getLogInfoTemplate(loggerAttribute));
    
    this.productStockAlertWrapper.sendMailAndNotification(requestId, username);
    return new GdnBaseRestResponse(null, null, true, requestId);
  }

  @RequestMapping(
      value = ProductStockAlertControllerPath.FIND_SKU_STOCK_ALERT_BY_BUSINESS_PARTNER_CODE,
      method = RequestMethod.GET, produces = {MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "send mail for item with stock alert",
      description = "send mail for item with stock alert")
  @ResponseBody
  public GdnRestSingleResponse<ProductStockAlertResponse> findSkuStockAlertByBusinessPartnerCode(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam(required = false) String username,
      @RequestParam(required = true) String businessPartnerCode) {
    
    LoggerAttributeModel loggerAttribute = new LoggerAttributeModel(this, "findSkuStockAlertByBusinessPartnerCode", null, 
        username, requestId, storeId, channelId, clientId, LoggerAspect.PRODUCT_LV3_FETCH, businessPartnerCode, null);
    
    MDC.put(LoggerParam.GENERIC_LOGGER.getParam(), LoggerStandard.getGenericLogTemplate(loggerAttribute));
    LOGGER.debug(LoggerStandard.getLogInfoTemplate(loggerAttribute));
    
    ProductStockAlertResponse reponses = new ProductStockAlertResponse();
    try {
      reponses.setItemSku(this.productStockAlertService.findGdnSkuStockAlertByBusinessPartnerCode(businessPartnerCode));
    } catch (Exception e) {
      LOGGER.error(LoggerStandard.appendLogErrorTemplate(e.getMessage(), e));
      throw e;
    }
    return new GdnRestSingleResponse<ProductStockAlertResponse>(null, null, true, reponses, requestId);
  }

  @AuditLog
  @RequestMapping(value = ProductStockAlertControllerPath.HIDE_OOS_ITEMS, method = RequestMethod.GET,
      produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Hides product that have been oos for a certain period of days",
      description = "Hides product that have been oos for a certain period of days")
  @ResponseBody
  public GdnBaseRestResponse hideItemSkuByOOSDate(@RequestParam String storeId, @RequestParam String channelId,
      @RequestParam String clientId, @RequestParam String requestId, @RequestParam(required = false) String username,
      @RequestParam(required = false, defaultValue = "1000") int batchSize) {
    LoggerAttributeModel loggerAttribute =
        new LoggerAttributeModel(this, "hideItemSkuByOOSDate", null, username, requestId, storeId, channelId, clientId,
            LoggerAspect.PRODUCT_LV3_FETCH, null, null);
    MDC.put(LoggerParam.GENERIC_LOGGER.getParam(), LoggerStandard.getGenericLogTemplate(loggerAttribute));
    LOGGER.debug(LoggerStandard.getLogInfoTemplate(loggerAttribute));
    try{
      this.productStockAlertService.hideItemSkuByOOSDate(storeId, requestId, batchSize);
      return new GdnBaseRestResponse(null, null, true, requestId);
    } catch (Exception e) {
      LOGGER.error(LoggerStandard.appendLogErrorTemplate(e.getMessage(), e));
      return new GdnBaseRestResponse(e.getMessage(), ErrorCategory.UNSPECIFIED.getCode(), false, requestId);
    }
  }

}
