package com.gdn.aggregate.platform.module.product.listener.service.processor.raw;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import com.gdn.aggregate.modules.agp.engagement.common.util.model.param.SaveParam;
import com.gdn.aggregate.modules.agp.engagement.common.util.model.request.SaveRequest;
import com.gdn.aggregate.modules.agp.engagement.common.util.service.DBService;
import com.gdn.aggregate.modules.agp.engagement.common.util.util.LoggerUtil;
import com.gdn.aggregate.modules.agp.engagement.common.util.util.MainUtil;
import com.gdn.aggregate.modules.agp.engagement.common.util.util.ParamUtil;
import com.gdn.aggregate.platform.module.product.listener.constants.Collections;
import com.gdn.aggregate.platform.module.product.listener.model.raw.FlashsaleProduct;
import com.gdn.aggregate.platform.module.product.listener.model.raw.MasterDataItem;
import com.gdn.aggregate.platform.module.product.listener.util.ModuleProductUtil;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

import com.gdn.aggregate.platform.module.product.listener.model.raw.Item;
import com.gdn.aggregate.platform.module.product.listener.repository.raw.ItemRepository;
import reactor.core.publisher.Mono;

@Component
public class ItemService {

  private static final String SAVE_COMMAND = "saveItem";

  @Value("${module.domain.product.max-item-size:20}")
  private int maxItemSize = 20;

  @Autowired
  private DBService dbService;

  @Autowired
  private ItemRepository itemRepository;

  @Autowired
  private MasterDataItemService masterDataItemService;

  @Autowired
  private PickupPointService pickupPointService;

  /*Save*/
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

  public void setMandatory(Item item, SaveParam saveParam) {
    Optional.ofNullable(item)
        .ifPresent(val -> {
          Item existingItem = getExistingItemWithObject(val);
          val.setId(val.toId());
          val = toUpdatedItemCode(val,existingItem);
          val = completingMasterDataItem(val);
          val = toUpdatedTag(val,existingItem);
        });
  }

  private Item toUpdatedItemCode(Item item, Item existingItem) {
    Optional.ofNullable(item)
        .filter(itm -> StringUtils.isEmpty(itm.getItemCode()))
        .ifPresent(itm -> itm.setItemCode(ModuleProductUtil.toItemCode(existingItem)));
    return item;
  }

  private Item completingMasterDataItem(Item item) {
    Optional.ofNullable(item)
        .map(Item::getItemCode)
        .map(masterDataItemService::getExistingMasterDataItem)
        .map(mdi -> getCompleteMasterDataItem(item,mdi))
        .ifPresent(item::setMasterDataItem);
    return item;
  }

  private Item toUpdatedTag(Item item, Item existingItem) {
    Optional.ofNullable(ModuleProductUtil.getTag(existingItem))
        .ifPresent(item::setTag);
    return item;
  }
  /*End of Save*/

  /*Getters*/
  public Item getExistingItem(String id) {
    return Optional.ofNullable(id)
        .flatMap(itemRepository::findById)
        .orElse(null);
  }

  public Item getExistingItemWithObject(Item item) {
    return Optional.ofNullable(item)
        .map(Item::toId)
        .map(this::getExistingItem)
        .orElse(null);
  }

  public List<Item> getItemsByItemCode(String itemCode) {
    return Optional.ofNullable(itemCode)
        .map(itmCode -> {
          try(Stream<Item> itemStream = itemRepository.streamAllByItemCode(itmCode)) {
            return itemStream
                .filter(Objects::nonNull)
                .sorted(Comparator.comparing(item -> MainUtil.toNotNullString(item.toId())))
                .collect(Collectors.toList());
          }
        })
        .orElseGet(ArrayList::new);
  }

  public List<String> getItemSkusByProductSku(String productSku) {
    return Optional.ofNullable(productSku)
        .map(prdSku -> {
          try(Stream<Item> itemStream = itemRepository.streamAllByProductSkuAndArchivedFalseAndMarkForDeleteFalse(prdSku)) {
            return itemStream
                .filter(Objects::nonNull)
                .sorted(Comparator.comparing(item -> MainUtil.toNotNullString(item.toId())))
                .map(Item::getId)
                .collect(Collectors.toList());
          }
        })
        .filter(CollectionUtils::isNotEmpty)
        .orElseGet(() -> getDefaultItemSkusByProductSku(productSku));
  }

  private List<String> getDefaultItemSkusByProductSku(String productSku) {
    return Optional.ofNullable(productSku)
        .map(itemRepository::findFirstByProductSkuOrderByArchivedAscMarkForDeleteAsc)
        .map(Item::getId)
        .map(MainUtil::toList)
        .orElseGet(ArrayList::new);
  }

  public List<Item> getItemsByFlashsaleProduct(FlashsaleProduct flashsaleProduct) {
    return Optional.ofNullable(ModuleProductUtil.getFlashsaleProductItemSku(flashsaleProduct))
        .map(this::getExistingItem)
        .map(MainUtil::toList)
        .filter(CollectionUtils::isNotEmpty)
        .orElseGet(() -> getItemsByProductSku(ModuleProductUtil.getFlashsaleProductProductSku(flashsaleProduct)));
  }

  public List<Item> getItemsByProductSku(String productSku) {
    List<Item> result = new ArrayList<>();
    Optional.ofNullable(productSku)
        .map(prdSku -> pickupPointService.getMinItemSkuByProductSku(prdSku))
        .filter(itemSku -> Objects.isNull(ModuleProductUtil.getItem(result,itemSku)))
        .map(this::getExistingItem)
        .ifPresent(result::add);
    Optional.ofNullable(productSku)
        .map(prdSku -> itemRepository.findAllByProductSkuOrderByArchivedAscMarkForDeleteAsc(prdSku,MainUtil.toSafePageable(null,maxItemSize)))
        .orElseGet(ArrayList::new)
        .stream()
        .filter(item -> Objects.isNull(ModuleProductUtil.getItem(result, ModuleProductUtil.toItemSku(item))))
        .forEach(result::add);
    Optional.ofNullable(productSku)
        .map(prdSku -> pickupPointService.getMaxItemSkuByProductSku(prdSku))
        .filter(itemSku -> Objects.isNull(ModuleProductUtil.getItem(result,itemSku)))
        .map(this::getExistingItem)
        .ifPresent(result::add);
    return result;
  }

  private MasterDataItem getCompleteMasterDataItem(Item item, MasterDataItem masterDataItem) {
    if (isItemAlreadyHaveNewestMasterDataItem(item,masterDataItem)) {
      return item.getMasterDataItem();
    }
    if (item.isSync()) {
      return masterDataItem;
    } else {
      MasterDataItem newMdi = MainUtil.getOrDefault(item.getMasterDataItem(),new MasterDataItem());
      newMdi.setMasterDataItemImages(masterDataItem.getMasterDataItemImages());
      return newMdi;
    }
  }

  private boolean isItemAlreadyHaveNewestMasterDataItem(Item item, MasterDataItem masterDataItem) {
    return Optional.ofNullable(item)
        .map(Item::getMasterDataItem)
        .map(mdi -> masterDataItem.getTimestamp() < mdi.getTimestamp())
        .orElseGet(() -> false);
  }

  public List<Item> getItemsAfterChangeMasterDataItems(List<MasterDataItem> masterDataItems) {
    return Optional.ofNullable(masterDataItems)
        .orElseGet(ArrayList::new)
        .stream()
        .filter(Objects::nonNull)
        .map(this::getItemsAfterChangeMasterDataItem)
        .filter(CollectionUtils::isNotEmpty)
        .flatMap(List::stream)
        .collect(Collectors.toList());
  }

  private List<Item> getItemsAfterChangeMasterDataItem(MasterDataItem masterDataItem) {
    return Optional.ofNullable(masterDataItem)
        .map(MasterDataItem::getId)
        .map(this::getItemsByItemCode)
        .orElseGet(ArrayList::new)
        .stream()
        .filter(Objects::nonNull)
        .peek(item -> item.setMasterDataItem(getCompleteMasterDataItem(item,masterDataItem)))
        .collect(Collectors.toList());
  }

  public boolean isAllItemsArchivedTrue(String productSku) {
    return !itemRepository.existsByProductSkuAndArchivedFalse(productSku);
  }

  public boolean isAllItemsMarkForDeleteTrue(String productSku) {
    return !itemRepository.existsByProductSkuAndMarkForDeleteFalse(productSku);
  }
  /*End of Getters*/

}
