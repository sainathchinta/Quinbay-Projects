package com.gdn.x.product.service.cache;

import static com.gdn.common.base.GdnPreconditions.checkArgument;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.stream.Collectors;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Lazy;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.stereotype.Service;

import com.gdn.common.base.GdnPreconditions;
import com.gdn.x.product.constants.CommonConstants;
import com.gdn.x.product.constants.ErrorMessages;
import com.gdn.x.product.exception.ApiErrorCodes;
import com.gdn.x.product.exception.ApiIncorrectInputDataException;
import com.gdn.x.product.model.entity.Item;
import com.gdn.x.product.model.entity.ItemPickupPoint;
import com.gdn.x.product.model.entity.PristineDataItem;
import com.gdn.x.product.model.vo.ItemAndItemPickupPointVo;
import com.gdn.x.product.service.api.CacheItemHelperService;
import com.gdn.x.product.service.api.ItemCacheableService;
import com.gdn.x.product.service.api.ItemPickupPointService;
import com.gdn.x.product.service.api.ItemService;
import com.gdn.x.product.service.api.ObjectConverterService;

import lombok.extern.slf4j.Slf4j;

@Slf4j
@Service
public class ItemCacheableServiceImpl implements ItemCacheableService {

  private static final int FIRST_INDEX = 0;

  @Autowired
  private CacheItemHelperService cacheItemHelperService;

  @Autowired
  @Lazy
  private ItemService itemService;

  @Autowired
  private ItemPickupPointService itemPickupPointService;

  @Autowired
  private ObjectConverterService objectConverterService;

  @Value("${fetch.item.pickup.point.without.delivery.en}")
  private boolean fetchItemPickupPointWithoutDelivery;

  @Override
  public Item findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(String storeId, String itemSku,
    boolean combineOthersBundlings, boolean off2On, boolean fbbActivated) {
    return findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(storeId, itemSku, true, combineOthersBundlings,
        false, null, off2On, fbbActivated);
  }

  @Override
  public Item findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(String storeId, String itemSku,
    boolean fullFetch, boolean combineOthersBundlings, boolean instantPickup, String pickupPointCode, boolean off2On,
    boolean fbbActivated) {
    Item item = cacheItemHelperService
        .findCacheableByStoreIdAndItemSkuAndMarkForDeleteFalse(storeId, itemSku);
    if(Objects.isNull(item) || item.isMarkForDelete()) {
      return null;
    }
    if (off2On && !item.isOff2OnChannelActive()) {
      return null;
    }
    ItemPickupPoint itemPickupPoint = fbbActivated ?
      itemPickupPointService
        .findByStoreIdAndItemSkuAndFbbActivatedTrueAndMarkForDeleteFalse(storeId, itemSku) :
      itemPickupPointService.findByItemSkuAndDelivery(storeId, itemSku);

    if (Objects.nonNull(itemPickupPoint)) {
      objectConverterService.overrideL4DetailsFromL5(Arrays.asList(item), Arrays.asList(itemPickupPoint));
    }
    if (combineOthersBundlings) {
      this.setActivePromoBundlingsByPristineOrItemCode(storeId, item);
    }

    if (fullFetch && Objects.nonNull(itemPickupPoint)) {
      item = getAndSetPriceAndItemViewConfig(storeId, item, instantPickup, pickupPointCode, fbbActivated);
    }

    return item;
  }

  @Override
  public ItemAndItemPickupPointVo findItemByItemSkuAndPickupPointCodeAndMarkForDeleteFalse(String storeId
      , String itemSku, String pickupPointCode) {
    Item item = cacheItemHelperService.findCacheableByStoreIdAndItemSkuAndMarkForDeleteFalse(storeId, itemSku);
    checkArgument(Objects.nonNull(item),
        CommonConstants.RETURN_NOT_FOUND_OF_ITEM_SKU + itemSku);
    if (item.isArchived()) {
      throw new ApiIncorrectInputDataException(ApiErrorCodes.PRODUCT_IS_ARCHIVED.getErrorMessage()+itemSku,
          ApiErrorCodes.PRODUCT_IS_ARCHIVED.getErrorCode());
    }
    ItemPickupPoint itemPickupPoint = this.itemPickupPointService.findByItemSkuAndPickupPointCode(
        storeId, itemSku, pickupPointCode);
    checkArgument(Objects.nonNull(itemPickupPoint) && !itemPickupPoint.isMarkForDelete(),
        CommonConstants.RETURN_NOT_FOUND_OF_ITEM_SKU_PICKUP_POINT + itemSku + " " + pickupPointCode);
    this.setActivePromoBundlingsByPristineOrItemCode(storeId, item, itemPickupPoint);
    return new ItemAndItemPickupPointVo(Arrays.asList(item), Arrays.asList(itemPickupPoint), 1);
  }

  @Override
  public ItemAndItemPickupPointVo findItemAndItemPickPointByproductSkus(String storeId, List<String> productSkus,
      List<String> itemSku, boolean showDeleted, int page, int pageSize) {
    List<Item> items;
    if (showDeleted) {
      items = itemService.getItemsByStoreIdAndProductSkusOrItemSkusIn(storeId, productSkus, itemSku);
    } else {
      items =
          itemService.findItemsByStoreIdAndProductSkuInOrItemSkuInAndMarkForDeleteFalse(storeId, productSkus, itemSku);
    }
    checkArgument(CollectionUtils.isNotEmpty(items), CommonConstants.ITEMS_NOT_FOUND);
    Set<String> itemSkus = items.stream().map(Item::getItemSku).collect(Collectors.toSet());
    Page<ItemPickupPoint> itemPickupPoints;
    if (showDeleted) {
      itemPickupPoints = itemPickupPointService.findAllItemPickupPointByProductSkusOrItemSkus(storeId, productSkus,
          new ArrayList<>(itemSkus), page, pageSize);
    } else {
      itemPickupPoints = itemPickupPointService.findItemPickupPointByProductSkusOrItemSkus(storeId, productSkus,
          new ArrayList<>(itemSkus), page, pageSize);
    }
    checkArgument(CollectionUtils.isNotEmpty(itemPickupPoints.getContent()),
        CommonConstants.ITEM_PICKUP_PONITS_NOT_FOUND);
    ItemAndItemPickupPointVo itemAndItemPickupPointVo =
        new ItemAndItemPickupPointVo(items, itemPickupPoints.getContent(), itemPickupPoints.getTotalElements());
    setActivePromoBundlings(storeId, itemAndItemPickupPointVo);
    return itemAndItemPickupPointVo;
  }

  private void setActivePromoBundlings(String storeId, ItemAndItemPickupPointVo itemAndItemPickupPointVo) {
    HashMap<String, Item> itemSkuAndItemMap = new HashMap<>();
    itemAndItemPickupPointVo.getItem().forEach(item -> itemSkuAndItemMap.put(item.getItemSku(), item));
    itemAndItemPickupPointVo.getItemPickupPoints().stream()
        .filter(itemPickupPoint -> itemSkuAndItemMap.containsKey(itemPickupPoint.getItemSku())).forEach(
            itemPickupPoint -> setActivePromoBundlingsByPristineOrItemCode(storeId,
                itemSkuAndItemMap.get(itemPickupPoint.getItemSku()), itemPickupPoint));
  }

  @Override
  public Item findItemByStoreIdAndItemSku(String storeId, String itemSku, boolean fullFetch,
      boolean combineOthersBundlings, boolean instantPickup, String pickupPointCode, boolean off2On) {
    Item item = cacheItemHelperService
        .findCacheableByStoreIdAndItemSku(storeId, itemSku);
    if (off2On && !item.isOff2OnChannelActive()) {
      return null;
    }
    ItemPickupPoint itemPickupPoint = itemPickupPointService.findByItemSkuAndDelivery(storeId, itemSku);
    if (Objects.nonNull(itemPickupPoint)) {
      objectConverterService.overrideL4DetailsFromL5(Arrays.asList(item), Arrays.asList(itemPickupPoint));
    }
    if (combineOthersBundlings) {
      this.setActivePromoBundlingsByPristineOrItemCode(storeId, item);
    }
    if (fullFetch && Objects.nonNull(itemPickupPoint)) {
      item = getAndSetPriceAndItemViewConfig(storeId, item, instantPickup, pickupPointCode, false);
    }
    return item;
  }

  @Override
  public Map<String, Item> findItemsByStoreIdAndItemSkusAndMarkForDeleteFalse(String storeId, Set<String> itemSkus) {
    return cacheItemHelperService.getCacheableItemsByItemSkus(storeId, itemSkus);
  }

  @Override
  public List<Item> findItemsByStoreIdAndProductSku(String storeId, String productSku, boolean combineOthersBundlings,
      boolean off2On, boolean isMigrateAndSyncProduct) {
    List<Item> results =
        cacheItemHelperService.findCacheableByStoreIdAndProductSku(storeId, productSku);
    if (off2On) {
      List<Item> itemsWithOff2OnChannelInactive =
          results.stream().filter(item -> !item.isOff2OnChannelActive()).collect(Collectors.toList());
      results.removeAll(itemsWithOff2OnChannelInactive);
    }
    if (combineOthersBundlings) {
      results.forEach(item -> this.setActivePromoBundlingsByPristineOrItemCode(storeId, item));
    }
    return getAndSetPriceAndItemViewConfigBasedOnDelivery(storeId, results, false, isMigrateAndSyncProduct);
  }

  @Override
  public List<Item> findItemsByStoreIdAndProductSkuAndMarkForDeleteFalse(String storeId, String productSku,
    boolean combineOthersBundlings, boolean off2On, boolean fbbActivated) {
    List<Item> results = cacheItemHelperService
        .findCacheableByStoreIdAndProductSkuAndMarkForDeleteFalse(storeId, productSku);
    if(off2On){
      List<Item> itemsWithOff2OnChannelInactive =
          results.stream().filter(item -> !item.isOff2OnChannelActive()).collect(Collectors.toList());
      results.removeAll(itemsWithOff2OnChannelInactive);
    }
    if (combineOthersBundlings) {
      results.forEach(item -> this.setActivePromoBundlingsByPristineOrItemCode(storeId, item));
    }
      return getAndSetPriceAndItemViewConfigBasedOnDelivery(storeId, results, fbbActivated, false);
  }

  @Override
  public List<Item> findItemsByStoreIdAndProductSkuAndShowDeletedFlag(String storeId, String productSku,
      boolean combineOthersBundlings, boolean off2On, boolean showDeleted, Map<String, ItemPickupPoint> itemPickupPointMap) {
    List<Item> results;
    if (showDeleted) {
      results = cacheItemHelperService.findCacheableByStoreIdAndProductSku(storeId, productSku);
    } else {
      results = cacheItemHelperService.findCacheableByStoreIdAndProductSkuAndMarkForDeleteFalse(storeId, productSku);
    }
    getItemForItemPickupPoint(results, itemPickupPointMap);
    if(off2On){
      List<Item> itemsWithOff2OnChannelInactive =
          results.stream().filter(item -> !item.isOff2OnChannelActive()).collect(Collectors.toList());
      results.removeAll(itemsWithOff2OnChannelInactive);
    }
    if (combineOthersBundlings) {
      results.forEach(item -> this.setActivePromoBundlingsByPristineOrItemCode(storeId,
          item, itemPickupPointMap.get(item.getItemSku())));
    }
    return results;
  }

  //map is formed with itemSku and pp
  private void getItemForItemPickupPoint(List<Item> itemList,
    Map<String, ItemPickupPoint> itemPickupPointMap) {
    itemList.removeIf(item -> !itemPickupPointMap.containsKey(item.getItemSku()));
  }

  private Item getAndSetPriceAndItemViewConfig(String storeId, Item result, boolean instantPickup,
      String pickupPointCode, boolean fbbActivated) {
    if (result != null) {
      if (instantPickup && StringUtils.isNotBlank(pickupPointCode)) {
        ItemPickupPoint itemPickupPoint =
            itemPickupPointService.findByStoreIdAndItemSkuAndPickupPointCodeAndCncActiveAndMarkForDeleteFalse(storeId,
                result.getItemSku(), pickupPointCode, true);
        result.setItemViewConfigs(itemPickupPoint.getItemViewConfig());
        result.setPrice(itemPickupPoint.getPrice());
      } else {
        getAndSetPriceAndItemViewConfigBasedOnDelivery(storeId, Arrays.asList(result), fbbActivated, false);
      }
    }
    return result;
  }

  private List<Item> getAndSetPriceAndItemViewConfigBasedOnDelivery(String storeId, List<Item> results,
    boolean fbbActivated, boolean isMigrateAndSyncProduct) {
    List<Item> newItems = new ArrayList<>();
    if (results != null) {
      List<ItemPickupPoint> itemPriceAndViewConfigs;
      if (fbbActivated) {
        itemPriceAndViewConfigs = itemPickupPointService
          .findByStoreIdAndItemSkuInAndMarkForDeleteFalseAndFbbActivated(storeId,
            new HashSet<>(getItemSkusByItems(results)), true);
      } else {
        itemPriceAndViewConfigs = getItemPickupPointsBasedOnDelivery(isMigrateAndSyncProduct, storeId, results);
      }

      Map<String, ItemPickupPoint> mapOfPriceAndViewConfigs = new HashMap<>();
      for (ItemPickupPoint itemPickupPoint : itemPriceAndViewConfigs) {
        mapOfPriceAndViewConfigs.put(itemPickupPoint.getItemSku(), itemPickupPoint);
      }
      for (Item item : results) {
        String itemSku = item.getItemSku();
        ItemPickupPoint itemPickupPoint = mapOfPriceAndViewConfigs.getOrDefault(itemSku, null);
        if (Objects.nonNull(itemPickupPoint)) {
          item.setPrice(itemPickupPoint.getPrice());
          item.setItemViewConfigs(itemPickupPoint.getItemViewConfig());
          newItems.add(item);
        }
      }
    }
    return newItems;
  }

  private List<ItemPickupPoint> getItemPickupPointsBasedOnDelivery(boolean isMigrateAndSyncProduct, String storeId,
      List<Item> results) {
    List<ItemPickupPoint> itemPriceAndViewConfigs;
    if (fetchItemPickupPointWithoutDelivery && isMigrateAndSyncProduct) {
      itemPriceAndViewConfigs = itemPickupPointService.findOneForEachItemSkuIn(storeId, getItemSkusByItems(results));
    } else {
      itemPriceAndViewConfigs =
          itemPickupPointService.findByItemSkusAndDelivery(storeId, getItemSkusByItems(results), true);
    }
    return itemPriceAndViewConfigs;
  }

  private List<String> getItemSkusByItems(List<Item> items) {
    List<String> itemSkus = new ArrayList<String>();
    for (Item item : items) {
      itemSkus.add(item.getItemSku());
    }
    return itemSkus;
  }

  @Override
  public List<Item> findItemsByPristineId(String storeId, String username, String requestId, String pristineId,
      boolean off2On) {
    Set<String> itemSkus = this.cacheItemHelperService
        .findCacheableItemSkusByStoreIdAndPristineIdAndMarkForDeleteFalse(storeId, pristineId);
    List<Item> cacheableItems = this.findItemsbyItemSkus(storeId, itemSkus, off2On);
    List<Item> items = cacheableItems.stream().filter(itemService::isItemBuyableAndDiscoverable)
        .collect(Collectors.toList());
    if (CollectionUtils.isEmpty(items)) {
      return itemService.getItemsWithDiscountPrice(storeId, username, requestId, cacheableItems);
    }
    return itemService.getItemsWithDiscountPrice(storeId, username, requestId, items);
  }

  @Override
  public List<Item> getItemsByPristineId(String storeId, String pristineId, boolean off2On) {
    GdnPreconditions.checkArgument(StringUtils.isNotBlank(storeId), ErrorMessages.STORE_ID_MUST_NOT_BE_BLANK);
    GdnPreconditions.checkArgument(StringUtils.isNotBlank(pristineId), ErrorMessages.PRISTINE_ID_MUST_NOT_BE_BLANK);
    Set<String> itemSkus =
        this.cacheItemHelperService.findCacheableItemSkusByStoreIdAndPristineIdAndMarkForDeleteFalse(storeId,
            pristineId);
    List<Item> cacheableItems = this.findItemsbyItemSkus(storeId, itemSkus, off2On);
    return cacheableItems;
  }

  @Override
  public List<Item> findItemsByStoreIdAndProductSkuAndCncActivatedAndMarkForDeleteFalse(String storeId,
      String productSku, boolean cncActivated) {
    List<Item> results =
        cacheItemHelperService.findCacheableByStoreIdAndProductSkuAndCncActivatedAndMarkForDeleteFalse(storeId,
            productSku, cncActivated);
    return getAndSetPriceAndItemViewConfigBasedOnDelivery(storeId, results, false, false);
  }

  @Override
  public List<Item> findAllItemByStoreIdAndItemCodeAndMarkForDeleteFalse(String storeId,
      String itemCode) {
    Set<String> itemSkus = cacheItemHelperService
        .findItemSkusByStoreIdAndItemCodeAndMarkForDeleteFalse(storeId, itemCode);
    return findItemsbyItemSkus(storeId, itemSkus, false);
  }

  @Override
  public Set<String> findAllItemSkusByStoreIdAndItemCodeAndMarkForDeleteFalse(String storeId,
      String itemCode) {
    return cacheItemHelperService
        .findItemSkusByStoreIdAndItemCodeAndMarkForDeleteFalse(storeId, itemCode);
  }

  /**
   * Get all items by itemSkus using multiget
   * @param storeId
   * @param itemSkus
   * @param off2On
   * @return
   */
  private List<Item> findItemsbyItemSkus(String storeId, Set<String> itemSkus, boolean off2On) {
    Map<String, Item> cachedItems =
        cacheItemHelperService.getCacheableItemsByItemSkus(storeId, itemSkus);
    List<Item> items = itemSkus.stream().map(itemSku -> cachedItems.containsKey(itemSku) ?
        cachedItems.get(itemSku) :
        this.findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(storeId, itemSku, Boolean.FALSE, Boolean.FALSE,
            Boolean.FALSE, null, false, false)).collect(Collectors.toList());
    if(off2On){
      items = items.stream().filter(item -> item.isOff2OnChannelActive()).collect(Collectors.toList());
    }
    return items;
  }

  @Override
  public Set<String> findItemCodesByPristine(String storeId, PristineDataItem pristineDataItem) {
    return cacheItemHelperService.findItemCodesByPristine(storeId, pristineDataItem);
  }

  @Override
  public Set<String> findCacheableItemCodesByStoreIdAndPristineAndMarkForDeleteFalse(String storeId,
                                                                                     String pristineId) {
    return cacheItemHelperService
        .findCacheableItemCodesByStoreIdAndPristineAndMarkForDeleteFalse(storeId, pristineId);
  }

  @Override
  public Set<String> findCacheableActivePromoBundlingsByStoreIdAndItemCode(String storeId,
                                                                           String itemCode) {
    return cacheItemHelperService
        .findCacheableActivePromoBundlingsByStoreIdAndItemCode(storeId, itemCode);
  }

  @Override
  public Set<String> findCacheableActivePromoBundlingsByStoreIdAndPristine(String storeId,
      PristineDataItem pristineDataItem) {
    return cacheItemHelperService
        .findCacheableActivePromoBundlingsByStoreIdAndPristine(storeId, pristineDataItem);
  }

  @Override
  public void setActivePromoBundlingsByPristineOrItemCode(String storeId, Item item) {
    if (item != null && item.isSynchronized()) {
      if (item.getPristineDataItem() != null) {
        item.setActivePromoBundlings(
            this.findCacheableActivePromoBundlingsByStoreIdAndPristine(storeId,
                item.getPristineDataItem()));
      } else if (StringUtils.isNotBlank(item.getItemCode())) {
        item.setActivePromoBundlings(
            this.findCacheableActivePromoBundlingsByStoreIdAndItemCode(storeId,
                item.getItemCode()));
      }
    }
  }

  private void setActivePromoBundlingsByPristineOrItemCode(String storeId, Item item, ItemPickupPoint itemPickupPoint) {
    if (item.isSynchronized()) {
      if (Objects.nonNull(item.getPristineDataItem())) {
        itemPickupPoint.setActivePromoBundlings(
            cacheItemHelperService.findCacheableActivePromoBundlingsByStoreIdAndPristineAndPickupPointCode(storeId,
                item.getPristineDataItem(), itemPickupPoint.getPickupPointCode()));
      } else if (StringUtils.isNotBlank(item.getItemCode())) {
        itemPickupPoint.setActivePromoBundlings(
            cacheItemHelperService.findCacheableActivePromoBundlingsByStoreIdAndItemCodeAndPickupPointCode(storeId,
                item.getItemCode(), itemPickupPoint.getPickupPointCode()));
      }
    }
  }

  @Override
  public List<Item> getCacheableItemsByProductSkus(String storeId, Set<String> productSkus){
    return cacheItemHelperService.getCacheableItemsByProductSkus(storeId, productSkus);
  }

  @Override
  public ItemAndItemPickupPointVo findItemAndItemPickPointByproductSkusAndCncActive(String storeId,
    List<String> productSkus, List<String> l5IdList, Boolean cncActivated, Integer page, Integer pageSize) {
    Page<ItemPickupPoint> itemPickupPoints;
    if (CollectionUtils.isNotEmpty(productSkus)) {
      itemPickupPoints =
        itemPickupPointService.findItemPickupPointByProductSkus(storeId, productSkus, cncActivated,
          page, pageSize);
    } else {
      List<ItemPickupPoint> itemPickupPointList =
        itemPickupPointService.findByStoreIdAndOfflineItemIds(storeId, l5IdList);
      itemPickupPoints = new PageImpl<>(itemPickupPointList, PageRequest.of(page, pageSize), itemPickupPointList.size());
    }
    if (CollectionUtils.isEmpty(itemPickupPoints.getContent())) {
      return null;
    }
    List<Item> items = itemService.findItemsByStoreIdAndItemSkuInAndMarkForDeleteFalse(storeId,
        itemPickupPoints.getContent().stream().map(ItemPickupPoint::getItemSku).collect(Collectors.toSet()));
    checkArgument(CollectionUtils.isNotEmpty(items), CommonConstants.ITEMS_NOT_FOUND);
    ItemAndItemPickupPointVo itemAndItemPickupPointVo =
        new ItemAndItemPickupPointVo(items, itemPickupPoints.getContent(), itemPickupPoints.getTotalElements());
    setActivePromoBundlings(storeId, itemAndItemPickupPointVo);
    return itemAndItemPickupPointVo;
  }
}
