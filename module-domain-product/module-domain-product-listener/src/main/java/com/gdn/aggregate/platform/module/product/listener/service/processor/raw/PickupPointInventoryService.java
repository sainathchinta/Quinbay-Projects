package com.gdn.aggregate.platform.module.product.listener.service.processor.raw;

import com.gdn.aggregate.modules.agp.engagement.common.util.model.param.SaveParam;
import com.gdn.aggregate.modules.agp.engagement.common.util.model.request.SaveRequest;
import com.gdn.aggregate.modules.agp.engagement.common.util.service.DBService;
import com.gdn.aggregate.modules.agp.engagement.common.util.util.LoggerUtil;
import com.gdn.aggregate.modules.agp.engagement.common.util.util.ParamUtil;
import com.gdn.aggregate.platform.module.product.listener.constants.Collections;
import com.gdn.aggregate.platform.module.product.listener.model.raw.PickupPointInventory;
import com.gdn.aggregate.platform.module.product.listener.model.raw.PickupPoint;
import com.gdn.aggregate.platform.module.product.listener.repositorysub.raw.ProductPickupPointInventoryRepository;
import com.gdn.aggregate.platform.module.product.listener.util.ModuleProductUtil;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections.CollectionUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import reactor.core.publisher.Mono;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.stream.Collectors;

@Component("ProductPickupPointInventoryService")
@Slf4j
public class PickupPointInventoryService {

  private static final String SAVE_COMMAND = "savePickupPointInventory";

  @Autowired
  private DBService dbService;

  @Autowired
  private ProductPickupPointInventoryRepository pickupPointInventoryRepository;


  /*Save*/
  public Mono<PickupPointInventory> save(PickupPointInventory pickupPointInventory, SaveParam saveParam) {
    if (Objects.isNull(pickupPointInventory.getId())) {
      String id = pickupPointInventory.toId();
      pickupPointInventory.setId(id);
    }
    String id = pickupPointInventory.getId();
    PickupPointInventory existing = getExistingPickupPointInventory(id);
    if (Objects.nonNull(existing)) {
      if (existing.getEventTimestamp() >= pickupPointInventory.getEventTimestamp()) {
        log.debug("Stale timestamp detected for {} - Existing: {}, New: {} - Rejecting update", id,
          existing.getEventTimestamp(), pickupPointInventory.getEventTimestamp());
        return Mono.just(existing);
      }
    } else {
      log.debug("No existing PickupPointInventory record found for {} - proceeding with insert", id);
    }
    SaveRequest<Object> request =
      SaveRequest.builder().index(Collections.PICKUP_POINT_INVENTORY).domain(pickupPointInventory)
        .clazz(PickupPointInventory.class).mongo(true).elasticsearch(false)
        .republish(ParamUtil.isRepublish(saveParam))
        .ignoreTimestamp(ParamUtil.isIgnoreTimestamp(saveParam)).build();

    return dbService.save(request).map(saved -> pickupPointInventory).doOnSuccess(s -> {
      LoggerUtil.sendSuccessExecuteLog(pickupPointInventory, SAVE_COMMAND, saveParam.getTraceId());
    }).doOnError(t -> {
      LoggerUtil.sendErrorExecuteLog(pickupPointInventory, t, SAVE_COMMAND, saveParam.getTraceId());
    });
  }


  public PickupPointInventory getExistingPickupPointInventory(String id) {
    try {
      return Optional.ofNullable(id)
          .flatMap(pickupPointInventoryRepository::findById)
          .orElse(null);
    } catch (Exception e) {
        log.error("[INVENTORY_FETCH_ERROR] Error fetching from product_pickup_point_inventory for id: {}", id, e);
        return null;
    }
  }

  public PickupPointInventory getExistingPickupPointInventory(String id,
    List<PickupPointInventory> pickupPointInventories) {
    if (Objects.nonNull(pickupPointInventories)) {
      PickupPointInventory cached = pickupPointInventories.stream()
        .filter(inv -> Objects.nonNull(inv) && Objects.equals(id, inv.getId()))
        .findFirst()
        .orElse(null);
      if (Objects.nonNull(cached)) {
        return cached;
      }
      return null;
    }
    return getExistingPickupPointInventory(id);
  }

  public List<PickupPointInventory> batchGetPickupPointInventories(List<PickupPoint> pickupPoints) {
    if (Objects.isNull(pickupPoints) || pickupPoints.isEmpty()) {
      return new ArrayList<>();
    }
    List<String> inventoryIds = pickupPoints.stream().filter(Objects::nonNull)
      .map(pp -> ModuleProductUtil.toPickupPointId(pp.getItemSku(), pp.getPickupPointCode()))
      .distinct().collect(Collectors.toList());
    try {
      List<PickupPointInventory> result = pickupPointInventoryRepository.findAllById(inventoryIds);
      return CollectionUtils.isNotEmpty(result) ? result : new ArrayList<>();
    } catch (Exception e) {
      log.error("[INVENTORY_BATCH_FETCH_ERROR] Error fetching from product_pickup_point_inventory", e);
      return new ArrayList<>();
    }
  }
}
