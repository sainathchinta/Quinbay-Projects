package com.gdn.mta.bulk.feignConfig;

import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.mta.bulk.dto.inventory.InventoryDetailInfoRequestDTO;
import com.gdn.mta.bulk.dto.inventory.InventoryDetailInfoResponseDTO;
import com.gdn.partners.pbp.dto.common.ListRequestDTO;
import feign.Headers;
import feign.Param;
import feign.RequestLine;

public interface InventoryFeign {

  @RequestLine("POST /inventoryInfo/findDetailByWebMerchantCodeAndWebItemSku?"
    + "storeId={storeId}&channelId={channelId}&clientId={clientId}&requestId={requestId}&username={username}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestListResponse<InventoryDetailInfoResponseDTO> findDetailByWebMerchantCodeAndWebItemSku(
    @Param("storeId") String storeId, @Param("channelId") String channelId,
    @Param("clientId") String clientId, @Param("requestId") String requestId,
    @Param("username") String username,
    ListRequestDTO<InventoryDetailInfoRequestDTO> inventoryDetailInfoRequestDTO);
}
