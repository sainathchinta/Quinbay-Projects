package com.gdn.partners.pcu.external.client.fallback;

import com.gdn.partners.pcu.external.client.model.InventoryDetailStockInfoRequestDTO;
import com.gdn.partners.pcu.external.web.model.response.L2StockDetailResponse;
import com.gdn.partners.pcu.external.web.model.response.L3AndPickupPointStockAvailabilityResponse;
import org.springframework.stereotype.Component;

import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.partners.pcu.external.client.feign.XInventoryFeign;
import com.gdn.partners.pcu.external.model.ErrorMessages;
import com.gdn.x.inventory.v2.rest.web.model.transaction.request.InventoryDetailInfoRequestDTO;
import com.gdn.x.inventory.v2.rest.web.model.transaction.request.ListRequestDTO;
import com.gdn.x.inventory.v2.rest.web.model.transaction.request.WarehouseInventoryStockInfoRequestDTO;
import com.gdn.x.inventory.v2.rest.web.model.transaction.response.InventoryDetailInfoResponseDTO;
import com.gdn.x.inventory.v2.rest.web.model.transaction.response.InventoryStockInfoDTO;
import com.gdn.x.inventory.v2.rest.web.model.transaction.response.ReservedStockSummaryResponse;
import com.gdn.x.inventory.v2.rest.web.model.transaction.response.WarehouseInventoryDetailResponseDTO;

@Component
public class XInventoryFeignFallback implements XInventoryFeign {
  /**
   * @param warehouseItemSku
   * @return
   */
  @Override
  public GdnRestSingleResponse<L2StockDetailResponse> findStockAvailabilityByWarehouseItemSku(
     String warehouseItemSku) {
    return new GdnRestSingleResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE,
      ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, null);
  }

  @Override
  public GdnRestSingleResponse<ReservedStockSummaryResponse> reservedStockSummaryByWebSKU(String webItemSku,
      boolean fetchAll) {
    return new GdnRestSingleResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE,
        ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, null);
  }

  @Override
  public GdnRestListResponse<InventoryStockInfoDTO> findDetailByWebProductSkus(boolean includeDistributionStock,
      InventoryDetailStockInfoRequestDTO request) {
    return new GdnRestListResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE,
        ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, null, null);
  }

  @Override
  public GdnRestListResponse<InventoryDetailInfoResponseDTO> findDetailByWebMerchantCodeAndWebItemSku(
      ListRequestDTO<InventoryDetailInfoRequestDTO> inventoryDetailInfoRequestDTO) {
    return new GdnRestListResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE,
        ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, null, null);
  }

  @Override
  public GdnRestSingleResponse<ReservedStockSummaryResponse> reservedStockSummaryByWebSKUAndPickupPointCode(
      String webItemSku, String pickupPointCode, boolean fetchAll) {
    return new GdnRestSingleResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE,
        ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, null);
  }

  @Override
  public GdnRestListResponse<WarehouseInventoryDetailResponseDTO> findByWebItemSkusAndWarehouseCodes(
      ListRequestDTO<WarehouseInventoryStockInfoRequestDTO> request) {
    return new GdnRestListResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE,
        ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, null, null);
  }

  @Override
  public GdnRestListResponse<WarehouseInventoryDetailResponseDTO> getStockForNonDistributedWareHouseByItemSkusAndWareHouseCodes(
      boolean useWarehouseStock, ListRequestDTO<WarehouseInventoryStockInfoRequestDTO> request) {
    return new GdnRestListResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE,
        ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, null, null);
  }

  @Override
  public GdnRestSingleResponse<L3AndPickupPointStockAvailabilityResponse> getStockAvailabilityByL3AndPickupPoint(
    String storeId, String requestId, String channelId, String clientId, String username,
    String webProductSku, String pickupPointCode) {
    return new GdnRestSingleResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE,
      ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, null);
  }
}
