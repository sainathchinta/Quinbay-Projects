package com.gdn.partners.pbp.outbound.warehouse.feign;

import com.blibli.oss.backend.common.model.response.Response;
import com.gdn.warehouse.itemmaster.command.model.biilofmaterial.CreateUpdateBillOfMaterialRecipeCommandRequest;
import com.gdn.warehouse.itemmaster.webmodel.response.CreateUpdateBOMRecipeResponse;
import feign.Headers;
import feign.Param;
import feign.RequestLine;

public interface WareHouseFeign {

  @RequestLine("POST /api/warehouse-item-master/bill-of-material/_create-update?storeId={storeId}&channelId={channelId}"
      + "&clientId={clientId}&username={username}&requestId={requestId}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  Response<CreateUpdateBOMRecipeResponse> createUpdateProductBundle(@Param("storeId") String storeId,
      @Param("channelId") String channelId, @Param("clientId") String clientId, @Param("requestId") String requestId,
      @Param("username") String username,
    CreateUpdateBillOfMaterialRecipeCommandRequest createUpdateBillOfMaterialRecipeRequest);

}
