package com.gdn.x.product.service.cache;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.cache.annotation.CacheEvict;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.stereotype.Service;

import com.gdn.x.product.enums.CacheNames;
import com.gdn.x.product.enums.Constants;
import com.gdn.x.product.service.api.CacheEvictItemService;
import com.gdn.x.product.service.api.CacheItemHelperService;

import lombok.extern.slf4j.Slf4j;

@Service
@Slf4j
public class CacheEvictItemServiceImpl implements CacheEvictItemService {

  private static final String cacheKeySeparator = "-";

  private static final String cacheValueKeySeparator = ":";

  @Autowired
  @Qualifier("redisTemplate")
  private RedisTemplate<String, Object> redisTemplate;

  @Autowired
  private CacheItemHelperService cacheItemHelperService;

  @Override
  @CacheEvict(value = CacheNames.FIND_ITEM_BY_ITEM_SKU, key = "#storeId + '-' + #itemSku")
  public void evictFindItemByItemSku(String storeId, String itemSku) {
    // used for cache eviction only
  }

  @Override
  @CacheEvict(value = {CacheNames.FIND_ITEM_BY_MERCHANT_SKU}, key = "#storeId + '-' + #merchantSku")
  public void evictFindItemByMerchantSku(String storeId, String merchantSku) {
    // used for cache eviction only
  }

  @Override
  @CacheEvict(
      value = {CacheNames.FIND_ITEM_BY_PRODUCT_SKU, CacheNames.FIND_ITEM_BY_PRODUCT_SKU_ALL},
      key = "#storeId + '-' + #productSku")
  public void evictFindItemByProductSku(String storeId, String productSku) {
    // used for cache eviction only
  }

  @Override
  public void evictFindItemSkusByPristineId(String storeId, String pristineId) {
    String key = getItemCacheKey(storeId, pristineId);
    log.info("Item cache eviction requested for pristine item with id:{}", pristineId);
    redisTemplate.delete(key);
    log.info("Item cache eviction successful for pristine item with id:{}", pristineId);
    this.cacheItemHelperService.findCacheableItemSkusByStoreIdAndPristineIdAndMarkForDeleteFalse(storeId, pristineId);
  }

  private String getItemCacheKey(String storeId, String pristineId){
    StringBuilder itemCacheKey = new StringBuilder(
        CacheNames.FIND_ALL_ITEMS_BY_PRISTINE_ID);
    itemCacheKey.append(cacheValueKeySeparator)
        .append(storeId)
        .append(cacheKeySeparator)
        .append(pristineId);
    return itemCacheKey.toString();
  }

  @Override
  @CacheEvict(value = {
      CacheNames.FIND_ACTIVE_PROMO_BUNDLINGS_BY_ITEM_CODE}, key = "#storeId + '_' + #itemCode")
  public void evictFindAllActivePromoBundlingsByItemCode(String storeId, String itemCode) {
    //used for cache eviction only
  }

  @Override
  @CacheEvict(value = {
      CacheNames.FIND_ACTIVE_PROMO_BUNDLINGS_BY_ITEM_CODE_AND_PICKUP_POINT_CODE}, key = "#storeId + '_' + #itemCode + '_' + #pickupPointCode")
  public void evictFindAllActivePromoBundlingsByItemCode(String storeId, String itemCode, String pickupPointCode) {
    //used for cache eviction only
  }


  @Override
  @CacheEvict(value = {CacheNames.FIND_ACTIVE_PROMO_BUNDLINGS_BY_PRISTINE_ID}, key =
      "#storeId + '_' + #pristineId")
  public void evictFindAllActivePromoBundlingsByPristineId(String storeId,
      String pristineId) {
    //used for cache eviction only
  }

  @Override
  @CacheEvict(value = {
      CacheNames.FIND_ACTIVE_PROMO_BUNDLINGS_BY_PRISTINE_ID_AND_PICKUP_POINT_CODE}, key = "#storeId + '_' + #pristineId + '_' + #pickupPointCode")
  public void evictFindAllActivePromoBundlingsByPristineId(String storeId, String pristineId, String pickupPointCode) {
    //used for cache eviction only
  }


  @Override
  @CacheEvict(cacheManager = Constants.UNIQUE_ID_CHECK_CACHE_MANAGER,
      value = {CacheNames.UNIQUE_ID_TYPE_CHECK}, key = "#storeId + '-' + #uniqueId")
  public void evictUniqueIdTypeCheck(String storeId, String uniqueId) {
    //used for cache eviction only
  }

  @CacheEvict(value = {CacheNames.FIND_ALL_ITEM_BY_ITEM_CODE}, key = "#storeId + '_' + #itemCode")
  public void evictFindAllByStoreIdAndItemCode(String storeId, String itemCode) {
    //used for cache eviction only
  }

  @Override
  @CacheEvict(value = {CacheNames.FIND_ITEM_CODES_BY_PRISTINE_ID}, key = "#storeId + '_' + #pristineId")
  public void evictFindItemCodesByPristine(String storeId, String pristineId) {
    //used for cache eviction only
  }

  @Override
  @CacheEvict(value = {
      CacheNames.FIND_ITEM_PICKUP_POINT_BY_ITEM_SKU_AND_PICKUP_POINT_CODE}, key = "#storeId + '_' + #itemSku +  '_' + #pickupPointCode")
  public void evictFindItemPickupPointByItemSkuAndPickupPointCode(String storeId, String itemSku,
      String pickupPointCode) {
    //used for cache eviction only
  }

  @Override
  @CacheEvict(value = CacheNames.FIND_ONE_L5_BY_ITEM_SKU, key = "#storeId + '_' + #itemSku")
  public void evictFindL5ByItemSku(String storeId, String itemSku) {
    // used for cache eviction only
  }
}
