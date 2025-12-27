package com.gdn.x.product.rest.web.controller.api;

import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.x.product.rest.web.model.ItemPickupPointMigrationApiPath;
import com.gdn.x.product.service.api.ItemPickupPointWrapperService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.bind.annotation.RestController;

import java.util.concurrent.ExecutorService;

@RestController
@Slf4j
@Tag(name = "ItemPickupPointMigration", description = "Item pickup point Migration API")
@RequestMapping(value = ItemPickupPointMigrationApiPath.ITEM_PICKUP_POINT_MIGRATION_BASE_PATH)
public class ItemPickupPointMigrationController {

  @Autowired
  private ItemPickupPointWrapperService itemPickupPointWrapperService;

  @Autowired
  private ExecutorService executorService;

  @RequestMapping(value = ItemPickupPointMigrationApiPath.MIGRATE_ITEM_PICKUP_POINT, method = RequestMethod.GET,
    produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Scheduler to migrate to new L5 collection",
    description = "Scheduler to migrate to new L5 collection")
  @ResponseBody
  public GdnBaseRestResponse migrateItemPickupPoint(@RequestParam String storeId,
    @RequestParam String channelId, @RequestParam String clientId,
    @RequestParam String requestId, @RequestParam(required = false) String username,
    @RequestParam(required = false) String itemSku, @RequestParam String status) {
    log.info("Scheduler to migrate to new L5 collection : {}", requestId);
    executorService.execute(
      () -> this.itemPickupPointWrapperService.migrateItemPickupPointCollection(storeId, itemSku, status));
    return new GdnBaseRestResponse(null, null, true, requestId);
  }
}
