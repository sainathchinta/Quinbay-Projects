package com.gdn.x.product.outbound.api.feign;

import com.blibli.oss.backend.common.model.response.Response;
import com.gdn.warehouse.itemmaster.command.model.biilofmaterial.CreateUpdateBillOfMaterialRecipeCommandRequest;
import com.gdn.warehouse.itemmaster.webmodel.response.CreateUpdateBOMRecipeResponse;
import com.gdn.x.product.outbound.api.feign.config.WarehouseFeignProperties;
import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestParam;

@FeignClient(name = "warehouseClient", url = "${api.warehouse.host}", configuration = WarehouseFeignProperties.class)
public interface WarehouseItemMasterFeign {
  @PostMapping(value = "/api/warehouse-item-master/bill-of-material/_create-update", consumes = MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
  Response<CreateUpdateBOMRecipeResponse> createUpdateProductBundle(
    @RequestParam("storeId") String storeId, @RequestParam("channelId") String channelId,
    @RequestParam("clientId") String clientId, @RequestParam("requestId") String requestId,
    @RequestParam("username") String username,
    @RequestBody CreateUpdateBillOfMaterialRecipeCommandRequest createUpdateBillOfMaterialRecipeRequest);

}

