package com.gdn.partners.pcu.external.client.fallback;

import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.partners.pcu.external.client.model.InventoryDetailStockInfoRequestDTO;
import com.gdn.partners.pcu.external.model.ErrorMessages;
import com.gdn.x.inventory.v2.rest.web.model.transaction.request.InventoryDetailInfoRequestDTO;
import com.gdn.x.inventory.v2.rest.web.model.transaction.request.ListRequestDTO;
import com.gdn.x.inventory.v2.rest.web.model.transaction.request.WarehouseInventoryStockInfoRequestDTO;
import com.gdn.x.inventory.v2.rest.web.model.transaction.response.InventoryDetailInfoResponseDTO;
import com.gdn.x.inventory.v2.rest.web.model.transaction.response.InventoryStockInfoDTO;
import com.gdn.x.inventory.v2.rest.web.model.transaction.response.ReservedStockSummaryResponse;
import com.gdn.x.inventory.v2.rest.web.model.transaction.response.WarehouseInventoryDetailResponseDTO;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;

public class XInventoryFeignFallbackTest {

  private XInventoryFeignFallback xInventoryFeignFallback = new XInventoryFeignFallback();
  private InventoryDetailStockInfoRequestDTO L3RequestDTO = new InventoryDetailStockInfoRequestDTO();
  private ListRequestDTO<InventoryDetailInfoRequestDTO> L4ListRequestDTO = new ListRequestDTO<>();
  private ListRequestDTO<WarehouseInventoryStockInfoRequestDTO> inventoryRequest = new ListRequestDTO<>();

  private static final String ITEM_SKU = "item-sku";
  private static final String PICKUP_POINT_CODE = "pp-code";
  private static final boolean IS_WARE_HOUSE = true;

  @Test
  public void reservedStockSummaryByWebSKUTest() {
    GdnRestSingleResponse<ReservedStockSummaryResponse> response =
        xInventoryFeignFallback.reservedStockSummaryByWebSKU(ITEM_SKU, IS_WARE_HOUSE);
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }

  @Test
  public void findDetailByWebProductSkusTest() {
    GdnRestListResponse<InventoryStockInfoDTO> response =
        xInventoryFeignFallback.findDetailByWebProductSkus(false,L3RequestDTO);
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }

  @Test
  public void findDetailByWebMerchantCodeAndWebItemSkuTest() {
    GdnRestListResponse<InventoryDetailInfoResponseDTO> response =
        xInventoryFeignFallback.findDetailByWebMerchantCodeAndWebItemSku(L4ListRequestDTO);
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }

  @Test
  public void reservedStockSummaryByWebSKUAndPickupPointCodeTest() {
    GdnRestSingleResponse<ReservedStockSummaryResponse> response =
        xInventoryFeignFallback.reservedStockSummaryByWebSKUAndPickupPointCode(ITEM_SKU, PICKUP_POINT_CODE,
            IS_WARE_HOUSE);
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }

  @Test
  public void findByWebItemSkusAndWarehouseCodesTest() {
    GdnRestListResponse<WarehouseInventoryDetailResponseDTO> response =
        xInventoryFeignFallback.findByWebItemSkusAndWarehouseCodes(inventoryRequest);
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }

  @Test
  public void getStockForNonDistributedWareHouseByItemSkusAndWareHouseCodesTest() {
    GdnRestListResponse<WarehouseInventoryDetailResponseDTO> response =
        xInventoryFeignFallback.getStockForNonDistributedWareHouseByItemSkusAndWareHouseCodes(true,inventoryRequest);
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }
}
