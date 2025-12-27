package com.gdn.partners.pbp.outbound.inventory.feign;

import com.gda.mta.product.dto.response.L2StockAvailabilityDTO;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.mta.product.entity.L2StockDetailResponse;
import com.gdn.mta.product.valueobject.InventoryStockInfoDTO;
import com.gdn.partners.pbp.dao.InventoryDetailInfoResponseV2DTO;
import com.gdn.partners.pbp.dao.UpdateSyncStockByWebItemPickupPointRequest;
import com.gdn.partners.pbp.dao.WebInventoryUpdateStockRequestV2DTO;
import com.gdn.x.inventory.v2.model.vo.response.UpdateSyncStockPPResponseDTO;
import com.gdn.x.inventory.v2.rest.web.model.transaction.request.InventoryDetailInfoRequestDTO;
import com.gdn.partners.pbp.dao.UpdatePoDateByL3RequestDTO;
import com.gdn.x.inventory.v2.rest.web.model.transaction.request.InventoryDetailStockInfoRequestDTO;
import com.gdn.x.inventory.v2.rest.web.model.transaction.request.ListRequestDTO;
import com.gdn.x.inventory.v2.rest.web.model.transaction.request.UpdateSyncStockByWebItemSkuRequestDTO;
import com.gdn.x.inventory.v2.rest.web.model.transaction.request.UpdateSyncStockByWebMerchantCodeRequestDTO;
import com.gdn.x.inventory.v2.rest.web.model.transaction.request.WebInventoryDeleteByWebItemSkuAndPickupPointCodeDTO;
import com.gdn.x.inventory.v2.rest.web.model.transaction.request.WebInventoryInsertRequestDTO;
import com.gdn.x.inventory.v2.rest.web.model.transaction.request.WebInventoryUpdateMinimumStockAlertRequestDTO;
import com.gdn.x.inventory.v2.rest.web.model.transaction.request.WebInventoryUpdatePickupPointCodeRequestDTO;
import com.gdn.x.inventory.v2.rest.web.model.transaction.request.WebInventoryUpdatePickupPointRequestDTO;
import com.gdn.x.inventory.v2.rest.web.model.transaction.response.InventoryDetailInfoResponseDTO;
import com.gdn.x.inventory.v2.rest.web.model.transaction.response.WebInventoryUpdatePickupPointResponseDTO;
import feign.Headers;
import feign.Param;
import feign.RequestLine;

public interface InventoryFeign {

  @RequestLine("POST /inventoryInfo/findDetailByWebMerchantCodeAndWebItemSku?"
    + "storeId={storeId}&channelId={channelId}&clientId={clientId}&requestId={requestId}&username={username}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestListResponse<InventoryDetailInfoResponseV2DTO> findDetailByWebMerchantCodeAndWebItemSku(
    @Param("storeId") String storeId, @Param("channelId") String channelId,
    @Param("clientId") String clientId, @Param("requestId") String requestId,
    @Param("username") String username,
    ListRequestDTO<InventoryDetailInfoRequestDTO> inventoryDetailInfoRequestDTO);

  @RequestLine("PUT /webInventory/updateSyncStockAtL5?"
      + "storeId={storeId}&channelId={channelId}&clientId={clientId}&requestId={requestId}&username={username}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestListResponse<UpdateSyncStockPPResponseDTO> updateSyncStockAtL5(@Param("storeId") String storeId,
      @Param("channelId") String channelId, @Param("clientId") String clientId, @Param("requestId") String requestId,
      @Param("username") String username,
      ListRequestDTO<UpdateSyncStockByWebItemPickupPointRequest> syncStockByWebItemPickupPointRequestDTOListRequestDTO);

  @RequestLine("POST /inventoryInfo/findDetailByWebProductSkus?storeId={storeId}"
      + "&channelId={channelId}&clientId={clientId}&requestId={requestId}&username={username}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestListResponse<InventoryStockInfoDTO> findDetailByWebProductSkus(
      @Param("storeId") String storeId, @Param("channelId") String channelId,
      @Param("clientId") String clientId, @Param("requestId") String requestId,
      @Param("username") String username, InventoryDetailStockInfoRequestDTO request);

  @RequestLine("POST /webInventory/updatePickupPoint?storeId={storeId}"
      + "&channelId={channelId}&clientId={clientId}&requestId={requestId}&username={username}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestSingleResponse<WebInventoryUpdatePickupPointResponseDTO> updatePickupPoint(
      @Param("storeId") String storeId, @Param("channelId") String channelId,
      @Param("clientId") String clientId, @Param("requestId") String requestId,
      @Param("username") String username, ListRequestDTO<WebInventoryUpdatePickupPointRequestDTO> request);

  @RequestLine("GET /webInventory/filterStock?storeId={storeId}&channelId={channelId}&clientId"
      + "={clientId}&requestId={requestId}&username={username}&webMerchantCode={webMerchantCode}&stockMin={stockMin}&stockMax={stockMax}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestListResponse<InventoryDetailInfoResponseDTO> filterWebInventoryStock(@Param("storeId") String storeId,
      @Param("channelId") String channelId, @Param("clientId") String clientId, @Param("requestId") String requestId,
      @Param("username") String username, @Param("webMerchantCode") String webMerchantCode,
      @Param("stockMin") Integer stockMin, @Param("stockMax") Integer stockMax);

  @RequestLine("POST /webInventory/insertByList?storeId={storeId}"
      + "&channelId={channelId}&clientId={clientId}&requestId={requestId}&username={username}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnBaseRestResponse insertWebInventoryByList(@Param("storeId") String storeId, @Param("channelId") String channelId,
      @Param("clientId") String clientId, @Param("requestId") String requestId, @Param("username") String username,
      ListRequestDTO<WebInventoryInsertRequestDTO> webInventoryInsertRequestDTOListRequestDTO);

  @RequestLine("PUT /webInventory/updateMinimumStockAlert?"
      + "storeId={storeId}&channelId={channelId}&clientId={clientId}&requestId={requestId}&username={username}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnBaseRestResponse updateMinimumStockAlertOfWebInventory(@Param("storeId") String storeId,
      @Param("channelId") String channelId, @Param("clientId") String clientId, @Param("requestId") String requestId,
      @Param("username") String username,
      WebInventoryUpdateMinimumStockAlertRequestDTO webInventoryUpdateMinimumStockAlertRequestDTO);

  @RequestLine("PUT /webInventory/updatePickupPointCode?"
      + "storeId={storeId}&channelId={channelId}&clientId={clientId}&requestId={requestId}&username={username}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnBaseRestResponse updatePickupPointCodeOfWebInventory(@Param("storeId") String storeId,
      @Param("channelId") String channelId, @Param("clientId") String clientId, @Param("requestId") String requestId,
      @Param("username") String username,
      WebInventoryUpdatePickupPointCodeRequestDTO webInventoryUpdatePickupPointCodeRequestDTO);

  @RequestLine("POST /webInventory/decreaseStock?storeId={storeId}"
      + "&channelId={channelId}&clientId={clientId}&requestId={requestId}&username={username}&publishHistory={publishHistory}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnBaseRestResponse decreaseWebInventoryStock(@Param("storeId") String storeId, @Param("channelId") String channelId,
      @Param("clientId") String clientId, @Param("requestId") String requestId, @Param("username") String username,
      @Param("publishHistory") boolean publishHistory,
      WebInventoryUpdateStockRequestV2DTO webInventoryUpdateStockRequestDTO);

  @RequestLine("POST /webInventory/increaseStock?storeId={storeId}"
      + "&channelId={channelId}&clientId={clientId}&requestId={requestId}&username={username}&publishHistory={publishHistory}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnBaseRestResponse increaseWebInventoryStock(@Param("storeId") String storeId, @Param("channelId") String channelId,
      @Param("clientId") String clientId, @Param("requestId") String requestId, @Param("username") String username,
      @Param("publishHistory") boolean publishHistory,
      WebInventoryUpdateStockRequestV2DTO webInventoryUpdateStockRequestV2DTO);

  @RequestLine("PUT /webInventory/updateSyncStockByWebMerchantCode?"
      + "storeId={storeId}&channelId={channelId}&clientId={clientId}&requestId={requestId}&username={username}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnBaseRestResponse updateSyncStockOfWebInventoryByWebMerchantCode(@Param("storeId") String storeId,
      @Param("channelId") String channelId, @Param("clientId") String clientId, @Param("requestId") String requestId,
      @Param("username") String username,
      UpdateSyncStockByWebMerchantCodeRequestDTO updateSyncStockByWebMerchantCodeRequestDTO);

  @RequestLine("PUT /webInventory/updateSyncStockByWebItemSku?"
      + "storeId={storeId}&channelId={channelId}&clientId={clientId}&requestId={requestId}&username={username}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnBaseRestResponse updateSyncStockOfWebInventoryByWebItemSku(@Param("storeId") String storeId,
      @Param("channelId") String channelId, @Param("clientId") String clientId, @Param("requestId") String requestId,
      @Param("username") String username,
      UpdateSyncStockByWebItemSkuRequestDTO updateSyncStockByWebItemSkuRequestDTO);

  @RequestLine("POST /webInventory/deleteByItemSkuAndPickupPointCode?storeId={storeId}"
      + "&channelId={channelId}&clientId={clientId}&requestId={requestId}&username={username}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnBaseRestResponse deleteByItemSkuAndPickupPointCode(@Param("storeId") String storeId,
      @Param("channelId") String channelId, @Param("clientId") String clientId, @Param("requestId") String requestId,
      @Param("username") String username, ListRequestDTO<WebInventoryDeleteByWebItemSkuAndPickupPointCodeDTO> request);

  @RequestLine("GET /inventoryInfo/findStockAvailabilityByWarehouseItemSku?storeId={storeId}"
    + "&channelId={channelId}&clientId={clientId}&requestId={requestId}&username={username}"
    + "&warehouseMerchantCode={warehouseMerchantCode}&warehouseItemSku={warehouseItemSku}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestSingleResponse<L2StockAvailabilityDTO> findStockAvailabilityByWarehouseItemSku(
    @Param("storeId") String storeId, @Param("channelId") String channelId,
    @Param("clientId") String clientId, @Param("requestId") String requestId,
    @Param("username") String username,
    @Param("warehouseMerchantCode") String warehouseMerchantCode,
    @Param("warehouseItemSku") String warehouseItemSku);

  @RequestLine(
      "GET /inventoryInfo/findStockAvailabilityByWarehouseItemSku?storeId={storeId}&channelId"
          + "={channelId}&clientId={clientId}&requestId={requestId}&username={username"
          + "}&warehouseItemSku={warehouseItemSku}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestSingleResponse<L2StockDetailResponse> getStockDetailsByWarehouseItemSku(
      @Param("storeId") String storeId, @Param("channelId") String channelId,
      @Param("clientId") String clientId, @Param("requestId") String requestId,
      @Param("username") String username, @Param("warehouseItemSku") String warehouseItemSku);

  @RequestLine("PUT /webInventory/updatePoDateByL3?"
      + "storeId={storeId}&channelId={channelId}&clientId={clientId}&requestId={requestId"
      + "}&username={username}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnBaseRestResponse updatePoDateByL3(@Param("storeId") String storeId,
      @Param("channelId") String channelId, @Param("clientId") String clientId,
      @Param("requestId") String requestId, @Param("username") String username,
      UpdatePoDateByL3RequestDTO updatePoDateByL3RequestDTO);
}
