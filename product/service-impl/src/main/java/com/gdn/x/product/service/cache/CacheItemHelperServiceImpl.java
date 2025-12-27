package com.gdn.x.product.service.cache;

import static com.gdn.common.base.GdnPreconditions.checkArgument;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.function.Function;
import java.util.stream.Collectors;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.cache.annotation.Cacheable;
import org.springframework.context.ApplicationContext;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.stereotype.Service;

import com.gdn.x.product.constants.CommonConstants;
import com.gdn.x.product.dao.api.ItemRepository;
import com.gdn.x.product.dao.api.PristineItemRepository;
import com.gdn.x.product.enums.CacheNames;
import com.gdn.x.product.model.entity.Item;
import com.gdn.x.product.model.entity.PristineDataItem;
import com.gdn.x.product.service.api.CacheItemHelperService;
import com.gdn.x.product.service.api.ItemPickupPointService;

@Service
public class CacheItemHelperServiceImpl implements CacheItemHelperService {

  private static final String STORE_ID_MUST_NOT_BE_BLANK = "Store id must not be blank";
  private static final String PRISTINE_ID_MUST_NOT_BE_BLANK = "pristineId must not be blank";
  private static final String ITEM_CODE_MUST_NOT_BE_BLANK = "itemCode must not be blank";

  @Autowired
  private ItemRepository itemRepository;

  @Autowired
  private PristineItemRepository pristineItemRepository;

  @Autowired
  @Qualifier("redisTemplate")
  private RedisTemplate<String, Item> redisTemplate;

  @Autowired
  private ApplicationContext applicationContext;

  @Autowired
  private ItemPickupPointService itemPickupPointService;

  @Override
  public Item findCacheableByStoreIdAndItemSkuAndMarkForDeleteFalse(String storeId, String itemSku) {
    Item item = getCacheItemHelperServiceBean().findCacheableByStoreIdAndItemSku(storeId, itemSku);
    if (Objects.nonNull(item) && item.isMarkForDelete()) {
      return null;
    }
    return item;
  }

  @Override
  @Cacheable(value = {CacheNames.FIND_ITEM_BY_ITEM_SKU}, key = "#storeId + '-' + #itemSku", unless = "#result == null")
  public Item findCacheableByStoreIdAndItemSku(String storeId, String itemSku) {
    return itemRepository.findItemByStoreIdAndItemSku(storeId, itemSku, false);
  }

  private CacheItemHelperService getCacheItemHelperServiceBean() {
   return applicationContext.getBean(CacheItemHelperService.class);
  }

  @Override
  public void setItemCacheByStoreIdAndItemSku(String storeId, String itemSku, Item item) {
    redisTemplate.opsForValue().set(getItemCacheKeyByItemSku(storeId, itemSku), item);
  }

  @Override
  @Cacheable(value = {
      CacheNames.FIND_ITEM_BY_PRODUCT_SKU_ALL}, key = "#storeId + '-' + #productSku", unless = "#result == null")
  public List<Item> findCacheableByStoreIdAndProductSku(String storeId, String productSku) {
    return itemRepository.findItemsByStoreIdAndProductSku(storeId, productSku);
  }

  @Override
  @Cacheable(value = {
      CacheNames.FIND_ITEM_BY_PRODUCT_SKU}, key = "#storeId + '-' + #productSku", unless = "#result == null")
  public List<Item> findCacheableByStoreIdAndProductSkuAndMarkForDeleteFalse(String storeId,
      String productSku) {
    return itemRepository.findItemsByStoreIdAndProductSkuAndMarkForDelete(storeId, productSku, Boolean.FALSE, false);
  }

  @Override
  @Cacheable(value = {
      CacheNames.FIND_ALL_ITEMS_BY_PRISTINE_ID}, key = "#storeId + '_' + #pristineId", unless = "#result == null")
  public Set<String> findCacheableItemSkusByStoreIdAndPristineIdAndMarkForDeleteFalse(String storeId,
      String pristineId) {
    checkArgument(StringUtils.isNotBlank(storeId),
        CacheItemHelperServiceImpl.STORE_ID_MUST_NOT_BE_BLANK);
    checkArgument(StringUtils.isNotEmpty(pristineId),
        CacheItemHelperServiceImpl.PRISTINE_ID_MUST_NOT_BE_BLANK);
    PristineDataItem pristineItem = this.pristineItemRepository.findByPristineId(pristineId);
    if (Objects.isNull(pristineItem)) {
      return Collections.EMPTY_SET;
    }
    return this.itemRepository.findItemSkusByPristine(storeId, pristineItem);
  }

  @Override
  @Cacheable(value = {
      CacheNames.FIND_ITEM_BY_PRODUCT_SKU}, key = "#storeId + '-' + #productSku", unless = "#result == null")
  public List<Item> findCacheableByStoreIdAndProductSkuAndCncActivatedAndMarkForDeleteFalse(String storeId,
      String productSku, boolean cncActivated) {
    return itemRepository
        .findItemsByStoreIdAndProductSkuAndCncActivatedAndMarkForDeleteFalse(storeId, productSku,
            cncActivated);
  }

  @Cacheable(value = {
      CacheNames.FIND_ALL_ITEM_BY_ITEM_CODE}, key = "#storeId + '_' + #itemCode", unless = "#result == null")
  @Override
  public Set<String> findItemSkusByStoreIdAndItemCodeAndMarkForDeleteFalse(String storeId, String itemCode) {
    checkArgument(StringUtils.isNotBlank(storeId), CacheItemHelperServiceImpl.STORE_ID_MUST_NOT_BE_BLANK);
    checkArgument(StringUtils.isNotBlank(itemCode), CacheItemHelperServiceImpl.ITEM_CODE_MUST_NOT_BE_BLANK);
    return itemRepository.findItemSkusByStoreIdAndItemCodeAndMarkForDeleteFalse(storeId, itemCode);
  }

  @Override
  @Cacheable(value = {
      CacheNames.FIND_ITEM_CODES_BY_PRISTINE_ID}, key = "#storeId + '_' + #pristineDataItem.pristineId", unless = "#result == null")
  public Set<String> findItemCodesByPristine(String storeId, PristineDataItem pristineDataItem){
    checkArgument(StringUtils.isNotBlank(storeId), CacheItemHelperServiceImpl.STORE_ID_MUST_NOT_BE_BLANK);
    checkArgument(pristineDataItem != null, CacheItemHelperServiceImpl.PRISTINE_ID_MUST_NOT_BE_BLANK);
    return itemRepository.getItemCodesByPristine(storeId, pristineDataItem);
  }

  @Override
  @Cacheable(value = {
      CacheNames.FIND_ITEM_CODES_BY_PRISTINE_ID}, key = "#storeId + '_' + #pristineId", unless = "#result == null")
  public Set<String> findCacheableItemCodesByStoreIdAndPristineAndMarkForDeleteFalse(String storeId,
      String pristineId) {
    checkArgument(StringUtils.isNotBlank(storeId), CacheItemHelperServiceImpl.STORE_ID_MUST_NOT_BE_BLANK);
    checkArgument(StringUtils.isNotEmpty(pristineId),
        CacheItemHelperServiceImpl.PRISTINE_ID_MUST_NOT_BE_BLANK);
    PristineDataItem pristineDataItem = this.pristineItemRepository.findByPristineId(pristineId);
    Set<String> itemCodes = itemRepository.getItemCodesByPristine(storeId, pristineDataItem);
    return itemCodes;
  }

  @Override
  @Cacheable(value = {
      CacheNames.FIND_ACTIVE_PROMO_BUNDLINGS_BY_ITEM_CODE}, key = "#storeId + '_' + #itemCode", unless = "#result == null")
  public Set<String> findCacheableActivePromoBundlingsByStoreIdAndItemCode(String storeId,
      String itemCode) {
    checkArgument(StringUtils.isNotBlank(storeId),
        CacheItemHelperServiceImpl.STORE_ID_MUST_NOT_BE_BLANK);
    checkArgument(StringUtils.isNotBlank(itemCode),
        CacheItemHelperServiceImpl.ITEM_CODE_MUST_NOT_BE_BLANK);
    Set<String> itemSkus = itemRepository.findItemSkusByStoreIdAndItemCode(storeId, itemCode);
    if (CollectionUtils.isNotEmpty(itemSkus)) {
      return itemPickupPointService.getActivePromoBundlingByItemSkus(storeId, new ArrayList<>(itemSkus));
    }
    return new HashSet<>();
  }

  @Override
  @Cacheable(value = {
      CacheNames.FIND_ACTIVE_PROMO_BUNDLINGS_BY_ITEM_CODE_AND_PICKUP_POINT_CODE}, key = "#storeId + '_' + #itemCode + '_' + #pickupPointCode", unless = "#result == null")
  public Set<String> findCacheableActivePromoBundlingsByStoreIdAndItemCodeAndPickupPointCode(String storeId,
      String itemCode, String pickupPointCode) {
    checkArgument(StringUtils.isNotBlank(storeId),
        CacheItemHelperServiceImpl.STORE_ID_MUST_NOT_BE_BLANK);
    checkArgument(StringUtils.isNotBlank(itemCode),
        CacheItemHelperServiceImpl.ITEM_CODE_MUST_NOT_BE_BLANK);
    Set<String> itemSkus = itemRepository.findItemSkusByStoreIdAndItemCode(storeId, itemCode);
    if (CollectionUtils.isNotEmpty(itemSkus)) {
      return itemPickupPointService.getActivePromoBundlingByItemSkusAndPickupPointCode(storeId, new ArrayList<>(itemSkus), pickupPointCode);
    }
    return new HashSet<>();
  }

  @Override
  @Cacheable(value = {CacheNames.FIND_ACTIVE_PROMO_BUNDLINGS_BY_PRISTINE_ID}, key = "#storeId + '_' + "
      + "#pristineDataItem.getPristineId()", unless = "#result == null")
  public Set<String> findCacheableActivePromoBundlingsByStoreIdAndPristine(String storeId,
      PristineDataItem pristineDataItem) {
    checkArgument(StringUtils.isNotBlank(storeId),
        CacheItemHelperServiceImpl.STORE_ID_MUST_NOT_BE_BLANK);
    checkArgument(pristineDataItem != null,
        CacheItemHelperServiceImpl.PRISTINE_ID_MUST_NOT_BE_BLANK);
    Set<String> itemSkus = this.itemRepository.findItemSkusByPristine(storeId, pristineDataItem);
    if (CollectionUtils.isNotEmpty(itemSkus)) {
      return itemPickupPointService.getActivePromoBundlingByItemSkus(storeId, new ArrayList<>(itemSkus));
    }
    return new HashSet<>();
  }

  @Override
  @Cacheable(value = {CacheNames.FIND_ACTIVE_PROMO_BUNDLINGS_BY_PRISTINE_ID_AND_PICKUP_POINT_CODE}, key =
      "#storeId + '_' + " + "#pristineDataItem.getPristineId() + '_' + #pickupPointCode", unless = "#result == null")
  public Set<String> findCacheableActivePromoBundlingsByStoreIdAndPristineAndPickupPointCode(String storeId,
      PristineDataItem pristineDataItem, String pickupPointCode) {
    checkArgument(StringUtils.isNotBlank(storeId),
        CacheItemHelperServiceImpl.STORE_ID_MUST_NOT_BE_BLANK);
    checkArgument(pristineDataItem != null,
        CacheItemHelperServiceImpl.PRISTINE_ID_MUST_NOT_BE_BLANK);
    Set<String> itemSkus = this.itemRepository.findItemSkusByPristine(storeId, pristineDataItem);
    if (CollectionUtils.isNotEmpty(itemSkus)) {
      return itemPickupPointService.getActivePromoBundlingByItemSkusAndPickupPointCode(storeId, new ArrayList<>(itemSkus), pickupPointCode);
    }
    return new HashSet<>();
  }

  @Override
  public Map<String, Item> getCacheableItemsByItemSkus(String storeId, Set<String> itemSkus) {
    List<String> itemCacheKeys =
        itemSkus.stream().map(itemSku -> getItemCacheKeyByItemSku(storeId, itemSku)).collect(Collectors.toList());
    return redisTemplate.opsForValue().multiGet(itemCacheKeys).stream().filter(Objects::nonNull)
        .collect(Collectors.toMap(Item::getItemSku, Function.identity()));
  }

  /**
   * cache key creation to fetch item skus from cache
   * @param storeId
   * @param itemSku
   * @return
   */
  private String getItemCacheKeyByItemSku(String storeId, String itemSku) {
    return new StringBuilder(CacheNames.FIND_ITEM_BY_ITEM_SKU + CommonConstants.CACHE_SEPARATOR).append(storeId).append("-").append(itemSku).toString();
  }

  public List<Item> getCacheableItemsByProductSkus(String storeId, Set<String> productSkus){
    List<String> productSkuCacheKeys = productSkus.stream()
        .map(productSku -> getItemCacheKeyByProductSku(storeId, productSku)).collect(Collectors.toList());
    List<Item> cacheableItems = new ArrayList<>();
    for (Object cacheObject : redisTemplate.opsForValue().multiGet(productSkuCacheKeys)) {
      if (Objects.nonNull(cacheObject)) {
        cacheableItems.addAll((List<Item>) cacheObject);
      }
    }
    return cacheableItems;
  }

  public List<Item> getCacheableItemsByProductSkusForSuspensionList(String storeId, Set<String> productSkus) {
    List<String> productSkuCacheKeys =
        productSkus.stream().map(productSku -> getItemCacheKeyByProductSkuForSuspensionList(storeId, productSku))
            .collect(Collectors.toList());
    List<Item> cacheableItems = new ArrayList<>();
    for (Object cacheObject : redisTemplate.opsForValue().multiGet(productSkuCacheKeys)) {
      if (Objects.nonNull(cacheObject)) {
        cacheableItems.addAll((List<Item>) cacheObject);
      }
    }
    return cacheableItems;
  }

  /**
   * Get redis cache key for productSku
   *
   * @param storeId
   * @param productSku
   * @return
   */
  private String getItemCacheKeyByProductSkuForSuspensionList(String storeId, String productSku) {
    return new StringBuilder(CacheNames.FIND_ITEM_BY_PRODUCT_SKU_ALL).append(":").append(storeId)
        .append("-").append(productSku).toString();
  }

  /**
   * Get redis cache key for productSku
   * @param storeId
   * @param productSku
   * @return
   */
  private String getItemCacheKeyByProductSku(String storeId, String productSku){
    return new StringBuilder(CacheNames.FIND_ITEM_BY_PRODUCT_SKU).append(":").append(storeId)
        .append("-").append(productSku).toString();
  }

}