package com.gdn.partners.pcu.external.client.feign;

import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.partners.pcu.external.client.factory.XInventoryFeignFallbackFactory;
import com.gdn.partners.pcu.external.client.model.InventoryDetailStockInfoRequestDTO;
import com.gdn.partners.pcu.external.web.model.response.L2StockDetailResponse;
import com.gdn.partners.pcu.external.web.model.response.L3AndPickupPointStockAvailabilityResponse;
import com.gdn.x.inventory.v2.rest.web.model.transaction.request.InventoryDetailInfoRequestDTO;
import com.gdn.x.inventory.v2.rest.web.model.transaction.request.ListRequestDTO;
import com.gdn.x.inventory.v2.rest.web.model.transaction.request.WarehouseInventoryStockInfoRequestDTO;
import com.gdn.x.inventory.v2.rest.web.model.transaction.response.InventoryDetailInfoResponseDTO;
import com.gdn.x.inventory.v2.rest.web.model.transaction.response.InventoryStockInfoDTO;
import com.gdn.x.inventory.v2.rest.web.model.transaction.response.ReservedStockSummaryResponse;
import com.gdn.x.inventory.v2.rest.web.model.transaction.response.WarehouseInventoryDetailResponseDTO;
import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;

@FeignClient(name = "XInventoryFeign", url = "${service.inventory.endpoint}", fallbackFactory = XInventoryFeignFallbackFactory.class)
public interface XInventoryFeign {

  @GetMapping(value = "/stockReservation/summary", produces = MediaType.APPLICATION_JSON_VALUE)
  GdnRestSingleResponse<ReservedStockSummaryResponse> reservedStockSummaryByWebSKU(
      @RequestParam("webItemSku") String webItemSku, @RequestParam("fetchAll") boolean fetchAll);

  @RequestMapping(value = "/inventoryInfo/findDetailByWebProductSkus", method = RequestMethod.POST,
      produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  GdnRestListResponse<InventoryStockInfoDTO> findDetailByWebProductSkus(
      @RequestParam boolean includeDistributionStock ,@RequestBody InventoryDetailStockInfoRequestDTO request);

  @RequestMapping(value = "/inventoryInfo/findDetailByWebMerchantCodeAndWebItemSku", method = RequestMethod.POST,
      produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  GdnRestListResponse<InventoryDetailInfoResponseDTO> findDetailByWebMerchantCodeAndWebItemSku(
      @RequestBody ListRequestDTO<InventoryDetailInfoRequestDTO> inventoryDetailInfoRequestDTO);

  @GetMapping(value = "/stockReservation/summary", produces = MediaType.APPLICATION_JSON_VALUE)
  GdnRestSingleResponse<ReservedStockSummaryResponse> reservedStockSummaryByWebSKUAndPickupPointCode(
      @RequestParam("webItemSku") String webItemSku, @RequestParam("pickupPointCode") String pickupPointCode,
      @RequestParam("fetchAll") boolean fetchAll);

  @RequestMapping(value = "/warehouseInventory/findByWebItemSkusAndWarehouseCodes",
      method = RequestMethod.POST, produces = MediaType.APPLICATION_JSON_VALUE,
      consumes = MediaType.APPLICATION_JSON_VALUE)
  GdnRestListResponse<WarehouseInventoryDetailResponseDTO> findByWebItemSkusAndWarehouseCodes(
      @RequestBody ListRequestDTO<WarehouseInventoryStockInfoRequestDTO> request);

  @RequestMapping(value = "/warehouseInventory/findByWebItemSkusAndWarehouseCodes",
      method = RequestMethod.POST, produces = MediaType.APPLICATION_JSON_VALUE,
      consumes = MediaType.APPLICATION_JSON_VALUE)
  GdnRestListResponse<WarehouseInventoryDetailResponseDTO> getStockForNonDistributedWareHouseByItemSkusAndWareHouseCodes(
      @RequestParam("useWarehouseStock") boolean useWarehouseStock, @RequestBody ListRequestDTO<WarehouseInventoryStockInfoRequestDTO> request);

  @GetMapping(value = "/inventoryInfo/findStockAvailabilityByProductSkuAndPickupPoint", produces = MediaType.APPLICATION_JSON_VALUE)
  GdnRestSingleResponse<L3AndPickupPointStockAvailabilityResponse> getStockAvailabilityByL3AndPickupPoint(
    @RequestParam("storeId") String storeId, @RequestParam("requestId") String requestId,
    @RequestParam("channelId") String channelId, @RequestParam("clientId") String clientId,
    @RequestParam("username") String username, @RequestParam("webProductSku") String webProductSku,
    @RequestParam("pickupPointCode") String pickupPointCode);

  @GetMapping(value = "/inventoryInfo/findStockAvailabilityByWarehouseItemSku", produces = MediaType.APPLICATION_JSON_VALUE)
  GdnRestSingleResponse<L2StockDetailResponse> findStockAvailabilityByWarehouseItemSku(
    @RequestParam("warehouseItemSku") String warehouseItemSku);
}
