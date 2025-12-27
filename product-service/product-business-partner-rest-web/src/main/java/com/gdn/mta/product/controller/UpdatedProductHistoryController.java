package com.gdn.mta.product.controller;

import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.slf4j.MDC;
import com.gdn.mta.product.util.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.bind.annotation.RestController;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.gda.mta.product.dto.LogAuditTrailUpdatedOfflineProductResponse;
import com.gda.mta.product.dto.LogAuditTrailUpdatedProductBulkRequest;
import com.gda.mta.product.dto.LogAuditTrailUpdatedProductRequest;
import com.gda.mta.product.dto.LogAuditTrailUpdatedProductResponse;
import com.gda.mta.product.dto.response.SimpleBooleanResponse;
import com.gdn.common.web.param.PageableHelper;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.common.web.wrapper.response.PageMetaData;
import com.gdn.mta.product.entity.UpdatedProductHistory;
import com.gdn.mta.product.service.UpdatedProductHistoryService;
import com.gdn.mta.product.service.LogAuditTrailUpdatedOfflineProductService;
import com.gdn.mta.product.util.ConverterUtil;
import com.gdn.mta.product.web.model.UpdatedProductHistoryControllerPath;
import com.gdn.partner.pbp.logger.standar.LoggerAspect;
import com.gdn.partner.pbp.logger.standar.LoggerAttributeModel;
import com.gdn.partner.pbp.logger.standar.LoggerParam;
import com.gdn.partner.pbp.logger.standar.LoggerStandard;
import com.gdn.partners.pbp.commons.constants.Constants;

import io.swagger.v3.oas.annotations.tags.Tag;
import io.swagger.v3.oas.annotations.Operation;

@RestController
@RequestMapping(value = UpdatedProductHistoryControllerPath.BASE_PATH)
@Tag(name = "UpdatedProductHistoryController", description = "Updated Product History Service API")
public class UpdatedProductHistoryController {

  private static final Logger LOGGER = LoggerFactory.getLogger(ProductController.class);

  @Autowired
  private UpdatedProductHistoryService updatedProductHistoryService;

  @Autowired
  private LogAuditTrailUpdatedOfflineProductService offlineProductAuditService;

  @RequestMapping(value = UpdatedProductHistoryControllerPath.GET_PRD_UPD_AUDITLOGS, method = RequestMethod.GET, produces = {
      MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "get product product update logs by gdnsku", description = "get product product update logs by gdnsku")
  @ResponseBody
  public GdnRestListResponse<LogAuditTrailUpdatedProductResponse> getProductUpdateLogs(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam String username, @RequestParam(defaultValue = "0") Integer page,
      @RequestParam(defaultValue = "10") Integer size, @RequestParam String gdnSku) {
    LoggerAttributeModel loggerAttribute =
        new LoggerAttributeModel(this, "getProductUpdateLogs", null, username, requestId, storeId, channelId, clientId,
            LoggerAspect.AUDIT_FETCH, page + ":" + size + ":" + gdnSku, null);

    MDC.put(LoggerParam.GENERIC_LOGGER.getParam(), LoggerStandard.getGenericLogTemplate(loggerAttribute));
    LOGGER.debug(LoggerStandard.getLogInfoTemplate(loggerAttribute));

    Pageable pageable = PageRequest.of(page, size);
    Page<UpdatedProductHistory> logAuditTrailUpdatedProducts =
        this.updatedProductHistoryService.getAuditLogsForProduct(pageable, gdnSku);
    return new GdnRestListResponse<>(null, null, true,
        getLogAuditTrailUpdatedProductResponseWrapper(logAuditTrailUpdatedProducts),
        new PageMetaData(pageable.getPageSize(), pageable.getPageNumber(),
            logAuditTrailUpdatedProducts.getTotalElements()), requestId);
  }

  @RequestMapping(value = UpdatedProductHistoryControllerPath.SAVE, method = RequestMethod.POST, produces = {
      MediaType.APPLICATION_JSON_VALUE}, consumes = {MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "Log Audit Trail for UpdatedProduct", description = "Log Audit Trail for Updated Product")
  @ResponseBody
  private GdnBaseRestResponse saveLogAuditTrailUpdatedProduct(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam String username, @RequestBody LogAuditTrailUpdatedProductBulkRequest request)
      throws JsonProcessingException {
    LoggerAttributeModel loggerAttribute =
        new LoggerAttributeModel(this, "saveLogAuditTrailUpdatedProduct", null, username, requestId, storeId, channelId,
            clientId, LoggerAspect.AUDIT_SAVE, request.getId(), request.toString());

    MDC.put(LoggerParam.GENERIC_LOGGER.getParam(), LoggerStandard.getGenericLogTemplate(loggerAttribute));
    LOGGER.debug(LoggerStandard.getLogInfoTemplate(loggerAttribute));

    List<UpdatedProductHistory> auditTrailUpdatedProductList = new ArrayList<>();
    for (LogAuditTrailUpdatedProductRequest source : request.getLogAuditTrailUpdatedProductList()) {
      UpdatedProductHistory log = new UpdatedProductHistory();
      BeanUtils.copyProperties(source, log);
      auditTrailUpdatedProductList.add(log);
    }
    updatedProductHistoryService.updateProductHistoryDeltailList(auditTrailUpdatedProductList);
    return new GdnBaseRestResponse(null, null, true, requestId);
  }

  private List<LogAuditTrailUpdatedProductResponse> getLogAuditTrailUpdatedProductResponseWrapper(
      Page<UpdatedProductHistory> logAuditTrailUpdatedProducts) {
    List<LogAuditTrailUpdatedProductResponse> wrapper = new ArrayList<>();
    DateFormat dateFormat = new SimpleDateFormat("MM/dd/yyyy HH:mm:ss");
    for (UpdatedProductHistory updatedProductHistory : logAuditTrailUpdatedProducts.getContent()) {
      LogAuditTrailUpdatedProductResponse logAuditDtoBean = new LogAuditTrailUpdatedProductResponse();
      logAuditDtoBean.setActivity(updatedProductHistory.getActivity());
      logAuditDtoBean.setCreatedByLog(updatedProductHistory.getChangedBy());
      logAuditDtoBean.setCreatedDateLog(dateFormat.format(updatedProductHistory.getAccessTime()));
      logAuditDtoBean.setNewValue(updatedProductHistory.getNewValues());
      logAuditDtoBean.setOldValue(updatedProductHistory.getOldValues());
      wrapper.add(logAuditDtoBean);
    }
    return wrapper;
  }

  @RequestMapping(value = UpdatedProductHistoryControllerPath.CHECK_PRODUCT_PRICE_CHANGE, method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Check if price changed from timestamp by gdnsku", description = "Check if price changed from timestamp by gdnsku")
  @ResponseBody
  public GdnRestSingleResponse<SimpleBooleanResponse> checkProductPriceChange(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam String username, @RequestParam Long timeStampFromInMillis, @PathVariable("sku") String gdnSku) {
    LoggerAttributeModel loggerAttribute =
        new LoggerAttributeModel(this, "checkProductPriceChange", null, username, requestId, storeId, channelId,
            clientId, LoggerAspect.AUDIT_CHECK_PRICE_CHANGE, gdnSku + Constants.COLON + timeStampFromInMillis, null);
    MDC.put(LoggerParam.GENERIC_LOGGER.getParam(), LoggerStandard.getGenericLogTemplate(loggerAttribute));
    LOGGER.debug(LoggerStandard.getLogInfoTemplate(loggerAttribute));
    return new GdnRestSingleResponse<>(updatedProductHistoryService.isPriceChangedForSku(gdnSku, new Date(timeStampFromInMillis)),
        requestId);
  }

  @RequestMapping(value = UpdatedProductHistoryControllerPath.CHECK_PRODUCT_PRICE_CHANGE_FOR_L5, method =
      RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Check if price changed from timestamp by gdnsku and PP code", description = "Check if price "
      + "changed from timestamp by gdnsku and PP code")
  @ResponseBody
  public GdnRestSingleResponse<SimpleBooleanResponse> checkProductPriceChangeForL5(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam String username, @RequestParam Long timeStampFromInMillis, @PathVariable("sku") String gdnSku,
      @PathVariable("pickupPointCode") String pickupPointCode) {
    LoggerAttributeModel loggerAttribute =
        new LoggerAttributeModel(this, "checkProductPriceChange", null, username, requestId, storeId, channelId,
            clientId, LoggerAspect.AUDIT_CHECK_PRICE_CHANGE, gdnSku + Constants.COLON + timeStampFromInMillis, null);
    MDC.put(LoggerParam.GENERIC_LOGGER.getParam(), LoggerStandard.getGenericLogTemplate(loggerAttribute));
    LOGGER.debug(LoggerStandard.getLogInfoTemplate(loggerAttribute));
    return new GdnRestSingleResponse<>(
        updatedProductHistoryService.isPriceChangedForSkuAndPPCode(gdnSku, new Date(timeStampFromInMillis),
            pickupPointCode), requestId);
  }

  @RequestMapping(value = UpdatedProductHistoryControllerPath.GET_OFFLINE_PRD_UPD_AUDIT_LOGS, method = RequestMethod.GET, produces = {
      MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "get Offline Product Update Logs by itemSku and Pickup Point Code", description = "get Offline Product Update Logs by itemSku and Pickup Point Code")
  @ResponseBody
  public GdnRestListResponse<LogAuditTrailUpdatedOfflineProductResponse> getOfflineProductUpdateLogs(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam String username, @RequestParam(defaultValue = "0") Integer page,
      @RequestParam(defaultValue = "10") Integer size, @RequestParam String itemSku,
      @RequestParam String pickupPointCode) {
    LoggerAttributeModel loggerAttribute =
        new LoggerAttributeModel(this, "getOfflineProductUpdateLogs", null, username, requestId, storeId, channelId,
            clientId, LoggerAspect.AUDIT_FETCH, page + ":" + size + ":" + itemSku + ":" + pickupPointCode, null);

    MDC.put(LoggerParam.GENERIC_LOGGER.getParam(), LoggerStandard.getGenericLogTemplate(loggerAttribute));
    LOGGER.debug(LoggerStandard.getLogInfoTemplate(loggerAttribute));

    Pageable pageable = PageRequest.of(page, size);
    Page<UpdatedProductHistory> updatedProductHistoryDetails = this.updatedProductHistoryService
        .getOfflineProductHistoryByItemSkuAndPickupPointCode(storeId, itemSku, pickupPointCode, pageable);
    return new GdnRestListResponse<>(null, null, true,
        ConverterUtil.toLogAuditTrailUpdatedOfflineProductResponses(updatedProductHistoryDetails),
        new PageMetaData(pageable.getPageSize(), pageable.getPageNumber(),
            updatedProductHistoryDetails.getTotalElements()), requestId);
  }

  @RequestMapping(value = UpdatedProductHistoryControllerPath.DELETE_FROM_DB, method = RequestMethod.DELETE, produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Api to delete the values from log audit trail table", description = "Api to delete the values from log audit trail table")
  @ResponseBody
  public GdnBaseRestResponse deleteFromDb(@RequestParam(required = false) String storeId,
      @RequestParam(required = false) String channelId, @RequestParam(required = false) String clientId,
      @RequestParam String requestId, @RequestParam(required = false) String username) {
    LoggerAttributeModel loggerAttribute =
        new LoggerAttributeModel(this, "deleteFromDbAndSolr", null, username, requestId, storeId, channelId, clientId,
            LoggerAspect.AUDIT_DELETE, null, null);
    MDC.put(LoggerParam.GENERIC_LOGGER.getParam(), LoggerStandard.getGenericLogTemplate(loggerAttribute));
    LOGGER.debug(LoggerStandard.getLogInfoTemplate(loggerAttribute));
    updatedProductHistoryService.deleteFromDb(storeId);
    return new GdnBaseRestResponse(null, null, true, requestId);
  }

  @RequestMapping(value = UpdatedProductHistoryControllerPath.DELETE_FROM_SOLR, method = RequestMethod.DELETE, produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Api to delete the values from log audit trail table", description = "Api to delete the values from log audit trail table")
  @ResponseBody
  public GdnBaseRestResponse deleteFromSolr(@RequestParam(required = false) String storeId,
      @RequestParam(required = false) String channelId, @RequestParam(required = false) String clientId,
      @RequestParam String requestId, @RequestParam(required = false) String username) {
    LoggerAttributeModel loggerAttribute =
        new LoggerAttributeModel(this, "deleteFromDbAndSolr", null, username, requestId, storeId, channelId, clientId,
            LoggerAspect.AUDIT_DELETE, null, null);
    MDC.put(LoggerParam.GENERIC_LOGGER.getParam(), LoggerStandard.getGenericLogTemplate(loggerAttribute));
    LOGGER.debug(LoggerStandard.getLogInfoTemplate(loggerAttribute));
    updatedProductHistoryService.deleteFromSolr(storeId);
    return new GdnBaseRestResponse(null, null, true, requestId);
  }
}