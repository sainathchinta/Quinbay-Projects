package com.gdn.partners.pbp.repository.productlevel3;

import java.util.List;
import java.util.Objects;
import java.util.UUID;

import com.gdn.partners.pbp.dao.InventoryDetailInfoResponseV2DTO;
import com.gdn.partners.pbp.dao.UpdateSyncStockByWebItemPickupPointRequest;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.common.web.param.MandatoryRequestParam;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.mta.product.enums.ApiErrorCode;
import com.gdn.mta.product.util.GdnMandatoryRequestParameterUtil;
import com.gdn.partners.pbp.commons.constants.Constants;
import com.gdn.partners.pbp.dao.WebInventoryUpdateStockRequestV2DTO;
import com.gdn.partners.pbp.outbound.inventory.feign.InventoryFeign;
import com.gdn.x.inventory.v2.model.vo.response.UpdateSyncStockPPResponseDTO;
import com.gdn.x.inventory.v2.rest.web.model.transaction.request.InventoryBaseRequest;
import com.gdn.x.inventory.v2.rest.web.model.transaction.request.InventoryDetailInfoRequestDTO;
import com.gdn.x.inventory.v2.rest.web.model.transaction.request.ListRequestDTO;
import com.gdn.x.inventory.v2.rest.web.model.transaction.request.UpdateSyncStockByWebItemSkuRequestDTO;
import com.gdn.x.inventory.v2.rest.web.model.transaction.request.UpdateSyncStockByWebMerchantCodeRequestDTO;
import com.gdn.x.inventory.v2.rest.web.model.transaction.request.WebInventoryDeleteByWebItemSkuAndPickupPointCodeDTO;
import com.gdn.x.inventory.v2.rest.web.model.transaction.request.WebInventoryInsertRequestDTO;
import com.gdn.x.inventory.v2.rest.web.model.transaction.request.WebInventoryUpdateMinimumStockAlertRequestDTO;
import com.gdn.x.inventory.v2.rest.web.model.transaction.request.WebInventoryUpdatePickupPointCodeRequestDTO;
import com.gdn.x.inventory.v2.rest.web.model.transaction.request.WebInventoryUpdateStockRequestDTO;
import com.gdn.x.inventory.v2.rest.web.model.transaction.response.InventoryDetailInfoResponseDTO;

import lombok.extern.slf4j.Slf4j;

@Slf4j
@Service
public class ProductLevel3InventoryRepositoryBean implements ProductLevel3InventoryRepository {

  @Autowired
  private InventoryFeign inventoryFeign;

  @Override
  public List<InventoryDetailInfoResponseV2DTO> findDetailByBusinessPartnerCodeAndGdnSku(
      MandatoryRequestParam mandatoryRequestParam, List<InventoryDetailInfoRequestDTO> request) throws Exception {
    GdnRestListResponse<InventoryDetailInfoResponseV2DTO> response =
        this.inventoryFeign.findDetailByWebMerchantCodeAndWebItemSku(mandatoryRequestParam.getStoreId(),
            mandatoryRequestParam.getChannelId(), mandatoryRequestParam.getClientId(),
            mandatoryRequestParam.getRequestId(), mandatoryRequestParam.getUsername(), new ListRequestDTO<>(request));
    if (!response.isSuccess()) {
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED,
          "[" + response.getErrorCode() + "] " + response.getErrorMessage());
    }
    return response.getContent();
  }

  @Override
  public List<InventoryDetailInfoResponseDTO> findDetailByBusinessPartnerCodeAndStock(
      MandatoryRequestParam mandatoryRequestParam, String businessPartnerCode, Integer stock) throws Exception {
    GdnRestListResponse<InventoryDetailInfoResponseDTO> response =
        this.inventoryFeign.filterWebInventoryStock(mandatoryRequestParam.getStoreId(),
            mandatoryRequestParam.getChannelId(), mandatoryRequestParam.getClientId(),
            mandatoryRequestParam.getRequestId(), mandatoryRequestParam.getUsername(), businessPartnerCode, stock,
            stock);
    if (!response.isSuccess()) {
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED,
          "[" + response.getErrorCode() + "] " + response.getErrorMessage());
    }
    return response.getContent();
  }

  @Override
  public void insertInventory(MandatoryRequestParam mandatoryRequestParam, List<WebInventoryInsertRequestDTO> request)
      throws Exception {
    settingTrackingFieldsForInsertInventory(request);
    log.info("Inventory insert. request : {} ", request);
    GdnBaseRestResponse response = this.inventoryFeign.insertWebInventoryByList(mandatoryRequestParam.getStoreId(),
        mandatoryRequestParam.getChannelId(), mandatoryRequestParam.getClientId(), mandatoryRequestParam.getRequestId(),
        mandatoryRequestParam.getUsername(), new ListRequestDTO<>(request));
    if (!response.isSuccess()) {
      log.error("Error while adding items to inventory : {}", request, response.getErrorMessage());
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED,
          "[" + response.getErrorCode() + "]" + response.getErrorMessage());
    }
  }

  private static void settingTrackingFieldsForInsertInventory(
      List<WebInventoryInsertRequestDTO> webInventoryInsertRequestDTOList) {
    for (WebInventoryInsertRequestDTO request : webInventoryInsertRequestDTOList) {
      InventoryBaseRequest inventoryBaseRequest = Objects.nonNull(request.getInventoryBaseRequest()) ?
          request.getInventoryBaseRequest() :
          new InventoryBaseRequest();
      inventoryBaseRequest.setOriginator(Constants.CATALOG_ORIGINATOR);
      inventoryBaseRequest.setTransactionDescription(Constants.CREATE_WEB_STOCK);
      String channelId = StringUtils.isNotEmpty(GdnMandatoryRequestParameterUtil.getChannelId()) ?
          GdnMandatoryRequestParameterUtil.getChannelId() :
          Constants.DEFAULT_CHANNEL_ID;
      StringBuilder trackingId = new StringBuilder();
      trackingId.append(UUID.randomUUID());
      trackingId.append(Constants.HYPHEN);
      trackingId.append(channelId);
      trackingId.append(Constants.HYPHEN);
      trackingId.append(request.getWebItemSku());
      trackingId.append(Constants.HYPHEN);
      trackingId.append(request.getPickupPointCode());
      inventoryBaseRequest.setTrackingId(trackingId.toString());
      inventoryBaseRequest.setActionKey(Constants.INCREASE_ORIGINAL_AND_AVAILABLE_FOR_CREATE_WEB_STOCK);
      request.setInventoryBaseRequest(inventoryBaseRequest);
    }
  }

  @Override
  public void updateMinimumStockAlert(MandatoryRequestParam mandatoryRequestParam,
      WebInventoryUpdateMinimumStockAlertRequestDTO request) throws Exception {
    GdnBaseRestResponse response =
        this.inventoryFeign.updateMinimumStockAlertOfWebInventory(mandatoryRequestParam.getStoreId(),
            mandatoryRequestParam.getChannelId(), mandatoryRequestParam.getClientId(),
            mandatoryRequestParam.getRequestId(), mandatoryRequestParam.getUsername(), request);
    if (!response.isSuccess()) {
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED,
          "[" + response.getErrorCode() + "] " + response.getErrorMessage());
    }
  }

  @Override
  public void updatePickupPoint(MandatoryRequestParam mandatoryRequestParam,
      WebInventoryUpdatePickupPointCodeRequestDTO request) throws Exception {
    GdnBaseRestResponse response =
        this.inventoryFeign.updatePickupPointCodeOfWebInventory(mandatoryRequestParam.getStoreId(),
            mandatoryRequestParam.getChannelId(), mandatoryRequestParam.getClientId(),
            mandatoryRequestParam.getRequestId(), mandatoryRequestParam.getUsername(), request);
    if (!response.isSuccess()) {
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED,
          "[" + response.getErrorCode() + "]" + response.getErrorMessage());
    }
  }

  @Override
  public void updateStockDecrease(MandatoryRequestParam mandatoryRequestParam,
      WebInventoryUpdateStockRequestV2DTO request) throws Exception {
    settingTrackingFieldsForStockDecrease(request);
    log.info("Decrease stock update. request : {} ", request);
    GdnBaseRestResponse response = this.inventoryFeign.decreaseWebInventoryStock(mandatoryRequestParam.getStoreId(),
        mandatoryRequestParam.getChannelId(), mandatoryRequestParam.getClientId(), mandatoryRequestParam.getRequestId(),
        mandatoryRequestParam.getUsername(), false, request);
    if (!response.isSuccess()) {
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED,
          "[" + response.getErrorCode() + "]" + response.getErrorMessage());
    }
  }

  private static void settingTrackingFieldsForStockDecrease(WebInventoryUpdateStockRequestDTO request) {
    InventoryBaseRequest inventoryBaseRequest = Objects.nonNull(request.getInventoryBaseRequest()) ?
        request.getInventoryBaseRequest() :
        new InventoryBaseRequest();
    inventoryBaseRequest.setOriginator(Constants.CATALOG_ORIGINATOR);
    inventoryBaseRequest.setTransactionDescription(Constants.UPDATE_PRODUCT_STOCK);
    String channelId = StringUtils.isNotEmpty(GdnMandatoryRequestParameterUtil.getChannelId()) ?
        GdnMandatoryRequestParameterUtil.getChannelId() :
        Constants.DEFAULT_CHANNEL_ID;
    StringBuilder trackingId = new StringBuilder();
    trackingId.append(UUID.randomUUID());
    trackingId.append(Constants.HYPHEN);
    trackingId.append(channelId);
    trackingId.append(Constants.HYPHEN);
    trackingId.append(request.getWebItemSku());
    trackingId.append(Constants.HYPHEN);
    trackingId.append(request.getPickupPointCode());
    inventoryBaseRequest.setTrackingId(trackingId.toString());
    inventoryBaseRequest.setActionKey(Constants.DECREASE_ORIGINAL_AND_AVAILABLE_FOR_UPDATE_STOCK);
    request.setInventoryBaseRequest(inventoryBaseRequest);
  }

  @Override
  public void updateStockIncrease(MandatoryRequestParam mandatoryRequestParam,
      WebInventoryUpdateStockRequestV2DTO request) throws Exception {
    settingTrackingFieldsForStockIncrease(request);
    log.info("Increase stock update. request : {} ", request);
    GdnBaseRestResponse response = this.inventoryFeign.increaseWebInventoryStock(mandatoryRequestParam.getStoreId(),
        mandatoryRequestParam.getChannelId(), mandatoryRequestParam.getClientId(), mandatoryRequestParam.getRequestId(),
        mandatoryRequestParam.getUsername(), false, request);
    if (!response.isSuccess()) {
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED,
          "[" + response.getErrorCode() + "]" + response.getErrorMessage());
    }
  }

  private static void settingTrackingFieldsForStockIncrease(WebInventoryUpdateStockRequestDTO request) {
    InventoryBaseRequest inventoryBaseRequest = Objects.nonNull(request.getInventoryBaseRequest()) ?
        request.getInventoryBaseRequest() :
        new InventoryBaseRequest();
    inventoryBaseRequest.setOriginator(Constants.CATALOG_ORIGINATOR);
    inventoryBaseRequest.setTransactionDescription(Constants.UPDATE_PRODUCT_STOCK);
    String channelId = StringUtils.isNotEmpty(GdnMandatoryRequestParameterUtil.getChannelId()) ?
        GdnMandatoryRequestParameterUtil.getChannelId() :
        Constants.DEFAULT_CHANNEL_ID;
    StringBuilder trackingId = new StringBuilder();
    trackingId.append(UUID.randomUUID());
    trackingId.append(Constants.HYPHEN);
    trackingId.append(channelId);
    trackingId.append(Constants.HYPHEN);
    trackingId.append(request.getWebItemSku());
    trackingId.append(Constants.HYPHEN);
    trackingId.append(request.getPickupPointCode());
    inventoryBaseRequest.setTrackingId(trackingId.toString());
    inventoryBaseRequest.setActionKey(Constants.INCREASE_ORIGINAL_AND_AVAILABLE_FOR_UPDATE_STOCK);
    request.setInventoryBaseRequest(inventoryBaseRequest);
  }

  @Override
  public ApiErrorCode updateStockIncreaseV2(MandatoryRequestParam mandatoryRequestParam,
      WebInventoryUpdateStockRequestV2DTO request) throws Exception {
    settingTrackingFieldsForStockIncrease(request);
    log.info("Increase stock update V2. request : {} ", request);
    GdnBaseRestResponse response = this.inventoryFeign.increaseWebInventoryStock(mandatoryRequestParam.getStoreId(),
        mandatoryRequestParam.getChannelId(), mandatoryRequestParam.getClientId(), mandatoryRequestParam.getRequestId(),
        mandatoryRequestParam.getUsername(), false, request);
    ApiErrorCode apiErrorCode = null;
    if (!response.isSuccess() && ApiErrorCode.MAXIMUM_STOCK_LIMIT_EXCEEDED.name().equals(response.getErrorCode())) {
      apiErrorCode = ApiErrorCode.MAXIMUM_STOCK_LIMIT_EXCEEDED;
    } else if (!response.isSuccess()) {
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED,
          "[" + response.getErrorCode() + "]" + response.getErrorMessage());
    }
    return apiErrorCode;
  }

  @Override
  public void updateSyncStockByBusinessPartnerCode(MandatoryRequestParam mandatoryRequestParam,
      UpdateSyncStockByWebMerchantCodeRequestDTO request) throws Exception {
    GdnBaseRestResponse response =
        this.inventoryFeign.updateSyncStockOfWebInventoryByWebMerchantCode(mandatoryRequestParam.getStoreId(),
            mandatoryRequestParam.getChannelId(), mandatoryRequestParam.getClientId(),
            mandatoryRequestParam.getRequestId(), mandatoryRequestParam.getUsername(), request);
    if (!response.isSuccess()) {
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED,
          "[" + response.getErrorCode() + "]" + response.getErrorMessage());
    }
  }

  @Override
  public void updateSyncStockByBusinessPartnerCodeAndGdnSku(MandatoryRequestParam mandatoryRequestParam,
      UpdateSyncStockByWebItemSkuRequestDTO request) throws Exception {
    GdnBaseRestResponse response =
        this.inventoryFeign.updateSyncStockOfWebInventoryByWebItemSku(mandatoryRequestParam.getStoreId(),
            mandatoryRequestParam.getChannelId(), mandatoryRequestParam.getClientId(),
            mandatoryRequestParam.getRequestId(), mandatoryRequestParam.getUsername(), request);
    if (!response.isSuccess()) {
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED,
          "[" + response.getErrorCode() + "]" + response.getErrorMessage());
    }
  }

  @Override
  public List<InventoryDetailInfoResponseV2DTO> findByBusinessPartnerCodeAndItemSkuAndPickupPointCode(String storeId,
      String channelId, String clientId, String requestId, String username,
      List<InventoryDetailInfoRequestDTO> inventoryDetailInfoRequestDTOS) {
    GdnRestListResponse<InventoryDetailInfoResponseV2DTO> response =
        this.inventoryFeign.findDetailByWebMerchantCodeAndWebItemSku(storeId, channelId, clientId, requestId, username,
            new ListRequestDTO<>(inventoryDetailInfoRequestDTOS));
    if (!response.isSuccess()) {
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED,
          "[" + response.getErrorCode() + "]" + response.getErrorMessage());
    }
    return response.getContent();
  }

  @Override
  public void updateSyncStockAtL5(String storeId, String channelId, String clientId, String requestId, String username,
      List<UpdateSyncStockByWebItemPickupPointRequest> updateSyncStockByWebItemPickupPointRequestDTOList) {
    GdnRestListResponse<UpdateSyncStockPPResponseDTO> response =
        this.inventoryFeign.updateSyncStockAtL5(storeId, channelId, clientId, requestId, username,
            new ListRequestDTO<>(updateSyncStockByWebItemPickupPointRequestDTOList));
    if (!response.isSuccess()) {
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED,
          "[" + response.getErrorCode() + "]" + response.getErrorMessage());
    }
  }


  @Override
  public void deleteByItemSkuAndPickupPointCode(String storeId, String channelId, String clientId, String requestId,
      String username,
      List<WebInventoryDeleteByWebItemSkuAndPickupPointCodeDTO> deleteByWebItemSkuAndPickupPointCodeDTOS) {
    GdnBaseRestResponse response =
        this.inventoryFeign.deleteByItemSkuAndPickupPointCode(storeId, channelId, clientId, requestId, username,
            new ListRequestDTO<>(deleteByWebItemSkuAndPickupPointCodeDTOS));
    if (!response.isSuccess()) {
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED,
          "[" + response.getErrorCode() + "]" + response.getErrorMessage());
    }
  }
}
