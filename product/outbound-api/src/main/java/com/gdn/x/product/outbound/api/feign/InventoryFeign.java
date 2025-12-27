package com.gdn.x.product.outbound.api.feign;

import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.x.inventory.v2.rest.web.model.transaction.request.InventoryDetailStockInfoRequestDTO;
import com.gdn.x.inventory.v2.rest.web.model.transaction.request.ListRequestDTO;
import com.gdn.x.inventory.v2.rest.web.model.transaction.request.WebInventoryDeleteByWebItemSkuAndPickupPointCodeDTO;
import com.gdn.x.inventory.v2.rest.web.model.transaction.response.InventoryStockInfoDTO;
import com.gdn.x.product.model.InventoryStockAvailableResponseDTO;
import com.gdn.x.product.outbound.api.feign.config.InventoryFeignProperties;
import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestParam;

@FeignClient(name = "inventoryClient", url = "${inventory.feign.host}", configuration = InventoryFeignProperties.class)
public interface InventoryFeign {

  @PostMapping(value = "/inventoryInfo/findDetailByWebProductSkus", consumes =
    MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
  GdnRestListResponse<InventoryStockInfoDTO> findDetailByWebProductSkus(
    @RequestParam("storeId") String storeId, @RequestParam("channelId") String channelId,
    @RequestParam("clientId") String clientId, @RequestParam("requestId") String requestId,
    @RequestParam("username") String username,
    @RequestBody InventoryDetailStockInfoRequestDTO inventoryDetailStockInfoRequestDTO);

  @PostMapping(value = "/webInventory/deleteByItemSkuAndPickupPointCode", consumes =
    MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
  GdnBaseRestResponse deleteByItemSkuAndPickupPointCode(@RequestParam("storeId") String storeId,
    @RequestParam("channelId") String channelId, @RequestParam("clientId") String clientId,
    @RequestParam("requestId") String requestId, @RequestParam("username") String username,
    @RequestBody ListRequestDTO<WebInventoryDeleteByWebItemSkuAndPickupPointCodeDTO> request);

  @GetMapping(value = "/inventoryInfo/stockInfoAvailabilityByWebProductSku", consumes =
    MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
  GdnRestSingleResponse<InventoryStockAvailableResponseDTO> checkStockAvailable(
    @RequestParam("storeId") String storeId, @RequestParam("channelId") String channelId,
    @RequestParam("clientId") String clientId, @RequestParam("requestId") String requestId,
    @RequestParam("username") String username, @RequestParam("webProductSku") String productSku, @RequestParam("isPreOrder") boolean isPreOrder);

}
