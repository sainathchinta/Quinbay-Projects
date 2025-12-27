package com.gdn.partners.pbp.outbound.inventory;

import com.gda.mta.product.dto.response.L2StockAvailabilityDTO;
import com.gdn.common.web.param.MandatoryRequestParam;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.mta.product.valueobject.InventoryStockInfoDTO;
import com.gdn.partners.pbp.dao.InventoryDetailInfoResponseV2DTO;
import com.gdn.partners.pbp.dao.UpdatePoDateByL3RequestDTO;
import com.gdn.x.inventory.v2.rest.web.model.transaction.request.InventoryDetailInfoRequestDTO;
import com.gdn.x.inventory.v2.rest.web.model.transaction.request.InventoryDetailStockInfoRequestDTO;
import com.gdn.x.inventory.v2.rest.web.model.transaction.request.ListRequestDTO;
import com.gdn.x.inventory.v2.rest.web.model.transaction.request.WebInventoryUpdatePickupPointRequestDTO;
import com.gdn.x.inventory.v2.rest.web.model.transaction.response.WebInventoryUpdatePickupPointResponseDTO;

public interface InventoryOutbound {

  /**
   * find Inventory Details by product skus
   * @param mandatoryRequestParam
   * @param request
   * @return
   * @throws Exception
   */
  GdnRestListResponse<InventoryStockInfoDTO> findDetailByWebProductSkus(MandatoryRequestParam mandatoryRequestParam,
      InventoryDetailStockInfoRequestDTO request);

  /**
   * find Inventory Details by merchant sku and item sku
   *
   * @param mandatoryRequestParam
   * @param inventoryDetailInfoRequestDTO
   * @return
   */
  GdnRestListResponse<InventoryDetailInfoResponseV2DTO> findDetailByWebMerchantCodeAndWebItemSku(
      MandatoryRequestParam mandatoryRequestParam, ListRequestDTO<InventoryDetailInfoRequestDTO> inventoryDetailInfoRequestDTO);

  /**
   *
   * @param mandatoryRequestParam
   * @param request
   * @return
   */
  WebInventoryUpdatePickupPointResponseDTO updatePickupPoint(MandatoryRequestParam mandatoryRequestParam,
      ListRequestDTO<WebInventoryUpdatePickupPointRequestDTO> request);
  /**
   * Find Stock Availability by item code and merchant code
   * @param mandatoryRequestParam non null
   * @param warehouseMerchantCode non null
   * @param warehouseItemSku non null
   * @return L2StockAvailabilityDTO
   */
  L2StockAvailabilityDTO findStockAvailabilityByWarehouseItemSku(
    MandatoryRequestParam mandatoryRequestParam, String warehouseMerchantCode,
    String warehouseItemSku);

  /**
   *
   * @param mandatoryRequestParam
   * @param itemCode
   * @return
   */
  boolean isWarehouseStockPresent(MandatoryRequestParam mandatoryRequestParam, String itemCode);

  /**
   * Update pre-order (PO) end date for all corresponding L5 items by L3 product SKU.
   *
   * @param mandatoryRequestParam non null mandatory request context
   * @param request                non null DTO containing product SKU(s) and new end date
   * @throws Exception if update fails or remote API returns error
   */
  boolean updatePoDateByL3(MandatoryRequestParam mandatoryRequestParam, UpdatePoDateByL3RequestDTO request) throws Exception;
}
