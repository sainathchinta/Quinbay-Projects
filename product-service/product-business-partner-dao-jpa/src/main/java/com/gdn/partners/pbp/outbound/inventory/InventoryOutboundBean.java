package com.gdn.partners.pbp.outbound.inventory;

import java.util.Objects;

import com.gda.mta.product.dto.response.L2StockAvailabilityDTO;
import com.gdn.mta.product.entity.L2StockDetailResponse;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.common.web.param.MandatoryRequestParam;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.partners.pbp.dao.InventoryDetailInfoResponseV2DTO;
import com.gdn.partners.pbp.dao.UpdatePoDateByL3RequestDTO;
import com.gdn.mta.product.valueobject.InventoryStockInfoDTO;
import com.gdn.partners.pbp.outbound.inventory.feign.InventoryFeign;
import com.gdn.x.inventory.v2.rest.web.model.transaction.request.InventoryDetailInfoRequestDTO;
import com.gdn.x.inventory.v2.rest.web.model.transaction.request.InventoryDetailStockInfoRequestDTO;
import com.gdn.x.inventory.v2.rest.web.model.transaction.request.ListRequestDTO;
import com.gdn.x.inventory.v2.rest.web.model.transaction.request.WebInventoryUpdatePickupPointRequestDTO;
import com.gdn.x.inventory.v2.rest.web.model.transaction.response.WebInventoryUpdatePickupPointResponseDTO;
import lombok.extern.slf4j.Slf4j;

@Slf4j
@Component
public class InventoryOutboundBean implements InventoryOutbound {

  @Autowired
  private InventoryFeign inventoryFeign;

  private static final Logger LOGGER = LoggerFactory.getLogger(InventoryOutboundBean.class);

  @Override
  public GdnRestListResponse<InventoryStockInfoDTO> findDetailByWebProductSkus(MandatoryRequestParam mandatoryRequestParam,
      InventoryDetailStockInfoRequestDTO request) {
    GdnRestListResponse<InventoryStockInfoDTO> response =
        this.inventoryFeign.findDetailByWebProductSkus(mandatoryRequestParam.getStoreId(),
            mandatoryRequestParam.getChannelId(), mandatoryRequestParam.getClientId(),
            mandatoryRequestParam.getRequestId(), mandatoryRequestParam.getUsername(), request);
    if (!response.isSuccess()) {
      log.error("Failed to fetch details by web productSku for request : {}, error - ",
          request, response.getErrorMessage());
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED,
          "[" + response.getErrorCode() + "]" + response.getErrorMessage());
    }
    return response;
  }

  @Override
  public GdnRestListResponse<InventoryDetailInfoResponseV2DTO> findDetailByWebMerchantCodeAndWebItemSku(
      MandatoryRequestParam mandatoryRequestParam, ListRequestDTO<InventoryDetailInfoRequestDTO> inventoryDetailInfoRequestDTO) {
    GdnRestListResponse<InventoryDetailInfoResponseV2DTO> response =
        this.inventoryFeign.findDetailByWebMerchantCodeAndWebItemSku(mandatoryRequestParam.getStoreId(),
            mandatoryRequestParam.getChannelId(), mandatoryRequestParam.getClientId(),
            mandatoryRequestParam.getRequestId(), mandatoryRequestParam.getUsername(), inventoryDetailInfoRequestDTO);
    if (!response.isSuccess()) {
      log.error("Failed to fetch details by web merchantSku and itemSku for request : {}, error - ",
          inventoryDetailInfoRequestDTO, response.getErrorMessage());
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED,
          "[" + response.getErrorCode() + "]" + response.getErrorMessage());
    }
    return response;
  }

  @Override
  public WebInventoryUpdatePickupPointResponseDTO updatePickupPoint(MandatoryRequestParam mandatoryRequestParam,
      ListRequestDTO<WebInventoryUpdatePickupPointRequestDTO> request) {
    log.info("Updating pickup point codes in x-inventory db, request : {}", request);
    GdnRestSingleResponse<WebInventoryUpdatePickupPointResponseDTO> response = this.inventoryFeign.updatePickupPoint(
        mandatoryRequestParam.getStoreId(), mandatoryRequestParam.getChannelId(), mandatoryRequestParam.getClientId(),
            mandatoryRequestParam.getRequestId(), mandatoryRequestParam.getUsername(), request);
    if (!response.isSuccess()) {
      log.error("Failed to fetch details by web merchantSku and itemSku for request : {}, error - ",
          request, response.getErrorMessage());
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED,
          "[" + response.getErrorCode() + "]" + response.getErrorMessage());
    }
    return response.getValue();
  }


  @Override
  public L2StockAvailabilityDTO findStockAvailabilityByWarehouseItemSku(
    MandatoryRequestParam mandatoryRequestParam, String warehouseMerchantCode,
    String warehouseItemSku) {
    GdnRestSingleResponse<L2StockAvailabilityDTO> stockAvailabilityByWarehouseItemSku =
      this.inventoryFeign.findStockAvailabilityByWarehouseItemSku(
        mandatoryRequestParam.getStoreId(), mandatoryRequestParam.getChannelId(),
        mandatoryRequestParam.getClientId(), mandatoryRequestParam.getRequestId(),
        mandatoryRequestParam.getUsername(), warehouseMerchantCode, warehouseItemSku);
    if (!stockAvailabilityByWarehouseItemSku.isSuccess()) {
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED,
        "[" + stockAvailabilityByWarehouseItemSku.getErrorCode() + "]"
          + stockAvailabilityByWarehouseItemSku.getErrorMessage());
    }
    return stockAvailabilityByWarehouseItemSku.getValue();
  }



  @Override
  public boolean isWarehouseStockPresent(MandatoryRequestParam mandatoryRequestParam,
      String itemCode) {
    log.info("checking if warehouse stock is available for the warehouseItemSku: {}", itemCode);
    GdnRestSingleResponse<L2StockDetailResponse> response =
        inventoryFeign.getStockDetailsByWarehouseItemSku(mandatoryRequestParam.getStoreId(),
            mandatoryRequestParam.getChannelId(), mandatoryRequestParam.getClientId(),
            mandatoryRequestParam.getRequestId(), mandatoryRequestParam.getUsername(), itemCode);
    if (!response.isSuccess() || Objects.isNull(response.getValue())) {
      log.error(
          "Failed to check  warehouse stock is available for the warehouseItemSku : {}, error - ",
          itemCode, response.getErrorMessage());
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED,
          "[" + response.getErrorCode() + "]" + response.getErrorMessage());
    }
    return response.getValue().isDistributionWarehouseAvailable() || response.getValue()
        .isNonDistributionWarehouseAvailable();
  }

  @Override
  public boolean updatePoDateByL3(MandatoryRequestParam mandatoryRequestParam,
      UpdatePoDateByL3RequestDTO request) throws Exception {
    log.info("Updating PO end date by L3. request : {}", request);
    GdnBaseRestResponse response = this.inventoryFeign.updatePoDateByL3(
        mandatoryRequestParam.getStoreId(), mandatoryRequestParam.getChannelId(),
        mandatoryRequestParam.getClientId(), mandatoryRequestParam.getRequestId(),
        mandatoryRequestParam.getUsername(), request);
    if (!response.isSuccess()) {
      log.error("Failed to update PO end date by L3 for request : {}, error - {}", request,
          response.getErrorMessage());
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED,
          "[" + response.getErrorCode() + "]" + response.getErrorMessage());
    }
    return response.isSuccess();
  }
}
