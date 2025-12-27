package com.gdn.x.product.service.cache;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.function.Function;
import java.util.stream.Collectors;

import com.gdn.x.product.model.entity.ItemPickupPoint;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Lazy;
import org.springframework.data.redis.connection.RedisConnection;
import org.springframework.data.redis.connection.lettuce.LettuceConnectionFactory;
import org.springframework.data.util.Pair;
import org.springframework.stereotype.Service;

import com.gdn.x.product.model.entity.Item;
import com.gdn.x.product.model.entity.PristineDataItem;
import com.gdn.x.product.model.entity.Product;
import com.gdn.x.product.service.api.CacheEvictHelperService;
import com.gdn.x.product.service.api.CacheEvictItemService;
import com.gdn.x.product.service.api.CacheEvictProductService;
import com.gdn.x.product.service.api.ItemService;
import com.gdn.x.product.service.api.PristineCacheableService;
import lombok.extern.slf4j.Slf4j;

@Service
@Slf4j
public class CacheEvictHelperServiceImpl implements CacheEvictHelperService {

  @Autowired
  private CacheEvictItemService cacheEvictItemService;

  @Autowired
  private CacheEvictProductService cacheEvictProductService;

  @Autowired
  private PristineCacheableService pristineCacheableService;

  @Autowired
  @Lazy
  private ItemService itemService;

  @Value("${redis.template.removed}")
  private boolean redisTemplateRemoved;

  @Override
  public void evictItemData(String storeId, Item item) {
    if (item != null) {
      this.cacheEvictItemService.evictFindItemByItemSku(storeId, item.getItemSku());
      this.cacheEvictItemService.evictUniqueIdTypeCheck(storeId, item.getItemSku());
      this.cacheEvictItemService.evictFindItemByProductSku(storeId, item.getProductSku());
      this.cacheEvictItemService.evictFindAllActivePromoBundlingsByItemCode(storeId, item.getItemCode());
      if (!StringUtils.isEmpty(item.getItemCode())){
        this.cacheEvictItemService.evictFindAllByStoreIdAndItemCode(storeId, item.getItemCode());
      }
      if (item.getPristineDataItem() != null) {
        this.cacheEvictItemService
            .evictFindItemCodesByPristine(storeId, item.getPristineDataItem().getPristineId());
        this.cacheEvictItemService
            .evictFindItemSkusByPristineId(storeId, item.getPristineDataItem().getPristineId());
        this.cacheEvictItemService.evictFindAllActivePromoBundlingsByPristineId(storeId,
            item.getPristineDataItem().getPristineId());
        pristineCacheableService.evictPristineItemAndSiblingsCacheAndRebuild(storeId, item.getPristineDataItem());
      }
      String merchantSku = item.getMerchantSku();
      if (merchantSku != null) {
        this.cacheEvictItemService.evictFindItemByMerchantSku(storeId, merchantSku);
      }
    }
  }

  @Override
  public void evictItemCache(String storeId, Item item){
    if (item != null) {
      this.cacheEvictItemService.evictFindItemByItemSku(storeId, item.getItemSku());
      this.cacheEvictItemService.evictUniqueIdTypeCheck(storeId, item.getItemSku());
      this.cacheEvictItemService.evictFindItemByProductSku(storeId, item.getProductSku());
    }
  }

  @Override
  public void evictProductData(String storeId, Product product) {
    if (product != null) {
      if(redisTemplateRemoved){
        cacheEvictProductService.evictFindProductByProductCodeUsingCacheEvict(storeId,product.getProductCode());
      }
      else {
        this.cacheEvictProductService.evictFindProductByProductCode(storeId, product.getProductCode());
      }
      this.cacheEvictProductService.evictFindProductByProductSku(storeId, product.getProductSku());
    }
  }

  @Override
  public void evictPristineItemCache(String storeId, PristineDataItem pristineDataItem, List<Item> items){
    pristineCacheableService.evictPristineItemAndSiblingsCacheAndRebuild(storeId, pristineDataItem);
    cacheEvictItemService.evictFindItemSkusByPristineId(storeId, pristineDataItem.getPristineId());
    items.forEach(item -> {
      this.cacheEvictItemService.evictFindItemByItemSku(storeId, item.getItemSku());
      this.cacheEvictItemService.evictUniqueIdTypeCheck(storeId, item.getItemSku());
      this.cacheEvictItemService.evictFindItemByProductSku(storeId, item.getProductSku());
    });
  }

  @Override
  public void flushRedisDBByJedisConnectionFactory(LettuceConnectionFactory lettuceConnectionFactory) {
    RedisConnection redisConnection = null;
    try {
      redisConnection = lettuceConnectionFactory.getConnection();
      redisConnection.flushDb();
    } catch (Exception e) {
      log.error("#flushRedisDBByJedisConnectionFactory failed error ", e);
    } finally {
      if (Objects.nonNull(redisConnection)) {
        redisConnection.close();
      }
    }
  }

  @Override
  public void evictItemPickupPointData(String storeId, ItemPickupPoint itemPickupPoint,
    String pickupPointCode) {
    if (itemPickupPoint != null) {
      Item item = itemService.findByStoreIdAndItemSku(storeId, itemPickupPoint.getItemSku());
      this.cacheEvictItemService.evictFindItemPickupPointByItemSkuAndPickupPointCode(storeId,
          itemPickupPoint.getItemSku(), pickupPointCode);
      this.cacheEvictItemService.evictUniqueIdTypeCheck(storeId, itemPickupPoint.getItemSku());
      this.cacheEvictItemService.evictFindItemByProductSku(storeId, itemPickupPoint.getProductSku());
      this.cacheEvictItemService.evictFindL5ByItemSku(storeId, itemPickupPoint.getItemSku());
      if(Objects.nonNull(item)) {
        this.cacheEvictItemService.evictFindAllActivePromoBundlingsByItemCode(storeId,
            item.getItemCode(), itemPickupPoint.getPickupPointCode());
        if (Objects.nonNull(item.getPristineDataItem())) {
          this.cacheEvictItemService.evictFindAllActivePromoBundlingsByPristineId(storeId,
              item.getPristineDataItem().getPristineId(), itemPickupPoint.getPickupPointCode());
        }
      }
    }
  }

  @Override
  public void evictItemPickupPointData(List<ItemPickupPoint> itemPickupPointList, List<Item> itemList) {
    if (CollectionUtils.isNotEmpty(itemPickupPointList)) {
      String storeId = itemPickupPointList.getFirst().getStoreId();
      Map<String, Item> itemSkuToitemMap = Optional.ofNullable(itemList).orElseGet(ArrayList::new).stream()
          .collect(Collectors.toMap(Item::getItemSku, Function.identity(), (oldVal, newVal) -> newVal));
      Set<String> uniqueItemSkus =
          itemPickupPointList.stream().map(ItemPickupPoint::getItemSku).collect(Collectors.toSet());
      Set<String> uniqueProductSkus =
          itemPickupPointList.stream().map(ItemPickupPoint::getProductSku).collect(Collectors.toSet());
      Set<Pair<String, String>> uniqueItemCodeAndPickupPointCodes =
          getUniqueItemCodeAndPickupPointCodes(itemPickupPointList, itemSkuToitemMap);
      Set<Pair<String, String>> uniquePristineAndPickupPointCodes =
          getUniquePristineIdsAndPickupPointCodes(itemPickupPointList, itemSkuToitemMap);
      for (ItemPickupPoint itemPickupPoint : itemPickupPointList) {
        this.cacheEvictItemService.evictFindItemPickupPointByItemSkuAndPickupPointCode(itemPickupPoint.getStoreId(),
            itemPickupPoint.getItemSku(), itemPickupPoint.getPickupPointCode());
      }
      for (String itemSku : uniqueItemSkus) {
        this.cacheEvictItemService.evictFindL5ByItemSku(storeId, itemSku);
      }
      for (String productSku : uniqueProductSkus) {
        this.cacheEvictItemService.evictFindItemByProductSku(storeId, productSku);
      }
      for (Pair<String, String> itemCodeAndPickupPointCode : uniqueItemCodeAndPickupPointCodes) {
        this.cacheEvictItemService.evictFindAllActivePromoBundlingsByItemCode(storeId,
            itemCodeAndPickupPointCode.getFirst(), itemCodeAndPickupPointCode.getSecond());
      }
      for (Pair<String, String> pristineAndPickupPointCode : uniquePristineAndPickupPointCodes) {
        this.cacheEvictItemService.evictFindAllActivePromoBundlingsByPristineId(storeId,
            pristineAndPickupPointCode.getFirst(), pristineAndPickupPointCode.getSecond());
      }
    }
  }

  private Set<Pair<String, String>> getUniqueItemCodeAndPickupPointCodes(List<ItemPickupPoint> itemPickupPointList,
      Map<String, Item> itemSkuToitemMap) {
    Set<Pair<String, String>> uniqueItemCodeAndPickupPointCodes = new HashSet<>();
    for (ItemPickupPoint itemPickupPoint : itemPickupPointList) {
      Optional.ofNullable(itemSkuToitemMap.get(itemPickupPoint.getItemSku())).map(Item::getItemCode).ifPresent(
          itemCode -> uniqueItemCodeAndPickupPointCodes.add(Pair.of(itemCode, itemPickupPoint.getPickupPointCode())));
    }
    return uniqueItemCodeAndPickupPointCodes;
  }

  private Set<Pair<String, String>> getUniquePristineIdsAndPickupPointCodes(List<ItemPickupPoint> itemPickupPointList,
      Map<String, Item> itemSkuToitemMap) {
    Set<Pair<String, String>> uniquePristineAndPickupPointCodes = new HashSet<>();
    for (ItemPickupPoint itemPickupPoint : itemPickupPointList) {
      Optional.ofNullable(itemSkuToitemMap.get(itemPickupPoint.getItemSku())).map(Item::getPristineDataItem)
          .map(PristineDataItem::getPristineId).ifPresent(pristineId -> uniquePristineAndPickupPointCodes.add(
              Pair.of(pristineId, itemPickupPoint.getPickupPointCode())));
    }
    return uniquePristineAndPickupPointCodes;
  }

  @Override
  public void evictItemPickupPointCache(String storeId, List<Item> items, List<ItemPickupPoint> itemPickupPointList) {
    if (CollectionUtils.isNotEmpty(itemPickupPointList)) {
      Map<String, Item> itemSkuAndItemMap;
      if (CollectionUtils.isEmpty(items)) {
       itemSkuAndItemMap = Optional.ofNullable(itemService.findByStoreIdAndItemSkus(storeId, itemPickupPointList
          .stream().map(ItemPickupPoint::getItemSku).collect(Collectors.toSet()))).orElse(new ArrayList<>()).stream().collect(
          Collectors.toMap(Item::getItemSku, Function.identity()));
      } else {
      itemSkuAndItemMap = items.stream().collect(Collectors.toMap(Item::getItemSku, Function.identity()));
      }
      for (ItemPickupPoint itemPickupPoint : itemPickupPointList) {
        Item item = itemSkuAndItemMap.getOrDefault(itemPickupPoint.getItemSku(), null);
        this.cacheEvictItemService.evictFindItemPickupPointByItemSkuAndPickupPointCode(storeId,
            itemPickupPoint.getItemSku(), itemPickupPoint.getPickupPointCode());
          if (Objects.nonNull(item)){
          this.cacheEvictItemService.evictFindAllActivePromoBundlingsByItemCode(storeId,
              item.getItemCode(), itemPickupPoint.getPickupPointCode());
          if (Objects.nonNull(item.getPristineDataItem())) {
            this.cacheEvictItemService.evictFindAllActivePromoBundlingsByPristineId(storeId,
                item.getPristineDataItem().getPristineId(), itemPickupPoint.getPickupPointCode());
          }
        }
      }
      itemPickupPointList.stream().forEach(itemPickupPoint -> this.cacheEvictItemService.evictUniqueIdTypeCheck(storeId,
          itemPickupPoint.getOfflineItemId()));
      Set<String> productSkuSet =
          itemPickupPointList.stream().map(ItemPickupPoint::getProductSku).collect(Collectors.toSet());
      productSkuSet.stream()
          .forEach(productSku -> this.cacheEvictItemService.evictFindItemByProductSku(storeId, productSku));
      itemSkuAndItemMap.keySet().stream().forEach(
          itemSku -> this.cacheEvictItemService.evictFindL5ByItemSku(storeId, itemSku));
    }
  }
}
