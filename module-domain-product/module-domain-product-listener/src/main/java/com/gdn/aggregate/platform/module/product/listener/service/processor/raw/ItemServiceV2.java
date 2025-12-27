package com.gdn.aggregate.platform.module.product.listener.service.processor.raw;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.function.Predicate;
import java.util.stream.Collectors;

import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;

import com.gdn.aggregate.modules.agp.engagement.common.util.model.param.SaveParam;
import com.gdn.aggregate.modules.agp.engagement.common.util.model.request.SaveRequest;
import com.gdn.aggregate.modules.agp.engagement.common.util.service.DBService;
import com.gdn.aggregate.modules.agp.engagement.common.util.util.LoggerUtil;
import com.gdn.aggregate.modules.agp.engagement.common.util.util.MainUtil;
import com.gdn.aggregate.modules.agp.engagement.common.util.util.ParamUtil;
import com.gdn.aggregate.platform.module.product.listener.constants.Collections;
import com.gdn.aggregate.platform.module.product.listener.model.raw.Item;
import com.gdn.aggregate.platform.module.product.listener.model.raw.MasterDataItem;
import com.gdn.aggregate.platform.module.product.listener.repository.raw.ItemRepository;
import com.gdn.aggregate.platform.module.product.listener.util.ModuleProductUtil;
import reactor.core.publisher.Mono;

@Component
public class ItemServiceV2 {

  private static final String SAVE_COMMAND = "saveItem";

  @Autowired
  private ItemRepository itemRepository;

  @Autowired
  private DBService dbService;

  public List<Item> findAllByProductSku(String productSku) {
    return itemRepository.findAllByProductSku(productSku);
  }

  public Mono<Boolean> save(Item item, SaveParam saveParam) {
    return Mono.fromCallable(() -> SaveRequest.builder()
            .index(Collections.ITEM)
            .domain(item)
            .clazz(Item.class)
            .mongo(true)
            .elasticsearch(false)
            .republish(ParamUtil.isRepublish(saveParam))
            .ignoreTimestamp(ParamUtil.isIgnoreTimestamp(saveParam))
            .build())
        .flatMap(saveRequest -> dbService.save(saveRequest))
        .doOnSuccess(s -> LoggerUtil.sendSuccessExecuteLog(item,SAVE_COMMAND, saveParam.getTraceId()))
        .doOnError(t -> LoggerUtil.sendErrorExecuteLog(item,t,SAVE_COMMAND, saveParam.getTraceId()))
        .onErrorResume(MainUtil::errorResult);
  }

  public Item getExistingItem(String id, List<Item> allItem) {
    return Optional.ofNullable(allItem).orElseGet(ArrayList::new)
        .stream()
        .filter(item -> Optional.ofNullable(id).orElse(StringUtils.EMPTY).equals(item.getId()))
        .findFirst().orElse(null);
  }

  public Item getExistingItemWithObject(Item item, List<Item> allItem) {
    return Optional.ofNullable(item).map(Item::toId).map(id -> getExistingItem(id, allItem)).orElse(null);
  }

  public Item toUpdatedItemCode(Item item, Item existingItem) {
    Optional.ofNullable(existingItem).filter(itm -> StringUtils.isEmpty(itm.getItemCode()))
        .ifPresent(itm -> itm.setItemCode(ModuleProductUtil.toItemCode(existingItem)));
    return item;
  }

  public Item completingMasterDataItem(Item item, MasterDataItem masterDataItem) {
    Optional.ofNullable(masterDataItem).map(mdi -> getCompleteMasterDataItem(item, masterDataItem))
        .ifPresent(item::setMasterDataItem);
    return item;
  }

  public Item toUpdatedTag(Item item, Item existingItem) {
    Optional.ofNullable(ModuleProductUtil.getTag(existingItem))
        .ifPresent(item::setTag);
    return item;
  }

  public List<String> getItemSkusByProductSku(String productSku, List<Item> allItems) {
    return Optional.of(Optional.ofNullable(allItems).orElseGet(ArrayList::new)
        .stream()
        .filter(item -> Optional.ofNullable(productSku).orElse(StringUtils.EMPTY).equals(item.getProductSku()))
        .filter(Predicate.not(Item::isMarkForDelete))
        .filter(Predicate.not(Item::isArchived))
        .sorted(Comparator.comparing(item -> MainUtil.toNotNullString(item.toId())))
        .map(Item::getId)
        .collect(Collectors.toList()))
        .map(itemSkus -> CollectionUtils.isEmpty(itemSkus) ? getDefaultItemSkusByProductSku(productSku, allItems) : itemSkus)
        .orElseGet(() -> getDefaultItemSkusByProductSku(productSku, allItems));
  }

  public boolean isAllItemsArchivedTrue(String productSku, List<Item> allItems) {
    return Optional.ofNullable(allItems).orElseGet(ArrayList::new)
        .stream()
        .filter(item -> Optional.ofNullable(productSku).orElse(StringUtils.EMPTY).equals(item.getProductSku()))
        .noneMatch(Predicate.not(Item::isArchived));
  }

  public boolean isAllItemsMarkForDeleteTrue(String productSku, List<Item> allItems) {
    return Optional.ofNullable(allItems).orElseGet(ArrayList::new)
        .stream()
        .filter(item -> Optional.ofNullable(productSku).orElse(StringUtils.EMPTY).equals(item.getProductSku()))
        .noneMatch(Predicate.not(Item::isMarkForDelete));
  }

  private List<String> getDefaultItemSkusByProductSku(String productSku, List<Item> allItems) {
    return Optional.ofNullable(allItems).orElseGet(ArrayList::new)
        .stream()
        .filter(item -> Optional.ofNullable(productSku).orElse(StringUtils.EMPTY).equals(item.getProductSku()))
        .sorted(Comparator.comparing(Item::isArchived).thenComparing(Item::isMarkForDelete))
        .findFirst()
        .map(Item::getId)
        .map(MainUtil::toList)
        .orElseGet(ArrayList::new);
  }

  private MasterDataItem getCompleteMasterDataItem(Item item, MasterDataItem masterDataItem) {
    if (isItemAlreadyHaveNewestMasterDataItem(item, masterDataItem)) {
      return item.getMasterDataItem();
    }
    if (item.isSync()) {
      return masterDataItem;
    } else {
      MasterDataItem newMdi = MainUtil.getOrDefault(item.getMasterDataItem(), new MasterDataItem());
      newMdi.setMasterDataItemImages(masterDataItem.getMasterDataItemImages());
      return newMdi;
    }
  }

  private boolean isItemAlreadyHaveNewestMasterDataItem(Item item, MasterDataItem masterDataItem) {
    if (Objects.isNull(masterDataItem)) {
      return false;
    }
    return Optional.ofNullable(item).map(Item::getMasterDataItem)
        .map(mdi -> masterDataItem.getTimestamp() < mdi.getTimestamp()).orElse(false);
  }

  public Item findSingleItemById(String id) {
    return itemRepository.findById(id).orElse(null);
  }
}
