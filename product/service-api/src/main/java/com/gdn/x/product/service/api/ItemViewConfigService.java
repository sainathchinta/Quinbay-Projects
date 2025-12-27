package com.gdn.x.product.service.api;

import java.util.List;
import java.util.Set;
import com.gdn.x.product.model.entity.Item;
import com.gdn.x.product.model.entity.ItemBuyableSchedule;
import com.gdn.x.product.model.entity.ItemDiscoverableSchedule;
import com.gdn.x.product.model.entity.ItemViewConfig;
import com.gdn.x.product.rest.web.model.request.ItemViewConfigAndItemSkuRequest;

public interface ItemViewConfigService {

  boolean addItemViewConfig(String storeId, String itemSku, ItemViewConfig itemViewConfig)
      throws Exception;

  boolean addItemViewConfigByMerchantSkuAndMerchantCode(String storeId, String requestId,
      String username, String merchantSku, String merchantCode, ItemViewConfig itemViewConfig);

  boolean deleteItemViewConfig(String storeId, String itemSku, String channel) throws Exception;

  boolean updateBuyableSchedule(String storeId, String channel, String itemSku,
      ItemBuyableSchedule itemBuyableSchedule);

  boolean updateDefaultBuyable(String storeId, String channel, String itemSku, boolean buyable);

  boolean updateDefaultDiscoverable(String storeId, String channel, String itemSku,
      boolean discoverable);

  boolean updateDiscoverableSchedule(String storeId, String channel, String itemSku,
      ItemDiscoverableSchedule itemDiscoverableSchedule);

  boolean updateItemViewConfig(String storeId, String itemSku, ItemViewConfig itemViewConfig)
      throws Exception;

  /**
   * @param storeId
   * @param itemViewConfigAndItemSkuRequests
   * @param forceReview
   * @param isArchive
   * @param productSku
   * @param scheduleRemoval
   * @return
   * @throws Exception
   */
  List<Item> updateItemViewConfigAndForceReview(String storeId, List<ItemViewConfigAndItemSkuRequest> itemViewConfigAndItemSkuRequests,
      boolean forceReview, boolean isArchive, String productSku, boolean scheduleRemoval) throws Exception;

  boolean updateItemViewConfigByMerchantSkuAndMerchantCode(String storeId, String requestId,
      String username, String merchantSku, String merchantCode, ItemViewConfig newItemViewConfig);

  /**
   *
   * @param item
   * @param itemViewConfigs
   * @return
   */
  boolean isItemViewConfigChangeForExistingChannelChange(Item item,
      Set<ItemViewConfig> itemViewConfigs);

  /**
   * @param storeId
   * @param itemSku
   * @param buyable
   * @param discoverable
   */
  void updateProductItemViewConfig(String storeId, String itemSku, boolean buyable, boolean discoverable)
      throws Exception;
}
