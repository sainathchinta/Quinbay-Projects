package com.gdn.x.product.service.api;

import java.util.List;

import com.gdn.x.product.model.entity.ItemPickupPoint;

import com.gdn.x.product.model.entity.Item;
import com.gdn.x.product.model.entity.PristineDataItem;
import com.gdn.x.product.model.entity.Product;

import org.springframework.data.redis.connection.lettuce.LettuceConnectionFactory;

public interface CacheEvictHelperService {

  void evictItemData(String storeId, Item item);

  void evictProductData(String storeId, Product product);

  void evictItemCache(String storeId, Item item);

  /**
   * Evict cache when there is any upadte in pristineDataItem
   * @param storeId
   * @param pristineDataItem
   * @param items
   */
  void evictPristineItemCache(String storeId, PristineDataItem pristineDataItem, List<Item> items);

  /**
   * Flush db by lettuceConnectionFactory
   *
   * @param lettuceConnectionFactory
   */
  void flushRedisDBByJedisConnectionFactory(LettuceConnectionFactory lettuceConnectionFactory);

  /**
   * Evict cache when there is any update in pickup point code in L5
   *
   * @param storeId
   * @param itemPickupPoint
   * @param pickupPointCode
   */
  void evictItemPickupPointData(String storeId, ItemPickupPoint itemPickupPoint,
    String pickupPointCode);

  /**
   * Evict cache when there is any update in pickup point code in L5
   *
   * @param itemPickupPointList
   * @param itemList
   */
  void evictItemPickupPointData(List<ItemPickupPoint> itemPickupPointList, List<Item> itemList);

  /**
   * Evict itemPickupPoint cache when price is changed
   *
   * @param storeId
   * @param items
   * @param itemPickupPointList
   */
  void evictItemPickupPointCache(String storeId, List<Item> items , List<ItemPickupPoint> itemPickupPointList);
}
