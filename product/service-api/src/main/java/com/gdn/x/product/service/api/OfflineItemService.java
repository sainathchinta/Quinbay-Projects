package com.gdn.x.product.service.api;

import java.util.List;
import java.util.Set;

import com.gdn.x.product.model.entity.Item;
import com.gdn.x.product.model.vo.ItemPickupPointPriceVo;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;

import com.gdn.common.web.param.MandatoryRequestParam;
import com.gdn.x.product.model.entity.ItemPickupPoint;
import com.gdn.x.product.model.entity.OfflineItem;
import com.gdn.x.product.model.vo.ProductAndItemsVO;
import com.gdn.x.product.model.vo.UpsertOfflineItemPriceResponseVO;
import com.gdn.x.product.rest.web.model.request.UpdateOfflineItemPriceRequest;
import com.gdn.x.product.rest.web.model.request.UpsertOfflineItemRequest;

public interface OfflineItemService {

  OfflineItem findByItemSkuAndPickupPointCodeAndMarkForDeleteFalse(String storeId, String itemSku, String pickupPointCode);

  void publishOfflineItems(String storeId, int batchSize, List<String> itemSkus) throws Exception;

  void republishOfflineItemsByMerchantCodes(String storeId, int batchSize, List<String> merchantCodes) throws Exception;

  void updateMarkForDeleteByMerchantCodeAndPickupPointCode(String storeId, List<OfflineItem> offlineItems,
      boolean markForDelete, String merchantCode, String pickupPointCode);

  /**
   * update ItemPickupPoint by merchantCode and pickupPointCode
   * @param storeId
   * @param itemPickupPointList
   * @param merchantCode
   * @param pickupPointCode
   */
  void updateItemPickupPointForCNCDeactivation(String storeId, List<ItemPickupPoint> itemPickupPointList,
       String merchantCode, String pickupPointCode);

  /**
   * update ItemPickupPoint by merchantCode and pickupPointCode
   *
   * @param storeId
   * @param itemPickupPointList
   * @param merchantCode
   * @param pickupPointCode
   */
  void updateItemPickupPointForDeliveryDeactivation(String storeId,
      List<ItemPickupPoint> itemPickupPointList, String merchantCode, String pickupPointCode);

  void updateMarkForDeleteByItemSkuAndPickupPointCode(String storeId, List<OfflineItem> offlineItems,
      boolean markForDelete, String merchantCode, String pickupPointCode);

  void updateMarkForDeleteByMerchantCodeAndItemSku(String storeId, boolean markForDelete, Item item);

  /**
   *
   * @param mandatoryRequestParam
   * @param username
   * @param merchantCode
   * @param upsertOfflineItemRequests
   * @param
   * @return
   */
  List<UpsertOfflineItemPriceResponseVO> upsertOfflineItem(
      MandatoryRequestParam mandatoryRequestParam, String username, String merchantCode, List<UpsertOfflineItemRequest> upsertOfflineItemRequests);

  List<OfflineItem> findByMerchantCodeAndItemSku(String storeId, String merchantCode, String itemSku);

  OfflineItem findByOfflineItemIdAndMarkForDeleteFalse(String storeId, String offlineItemId);

  /**
   * Find offlineItems by offlineItemIds
   *
   * @param storeId
   * @param offlineItemId
   * @return
   */
  List<OfflineItem> findByOfflineItemIds(String storeId,
      List<String> offlineItemId);

  void deleteOfflineItemByMerchantCode(String storeId, String merchantCode, String username);

  void deleteByPickupPointCode(String storeId, String pickupPointCode);

  /**
   * delete by pickupPoint
   *
   * @param storeId
   * @param pickupPointCode
   */
  void deleteByPickupPointCode_ItemPickupPoint(String storeId, String pickupPointCode);

  void validateAndUpdateProductItemCncActivatedFalse(String storeId, List<OfflineItem> offlineItems, String username,
      String solrUsername);

  /**
   * Validate and update cnc flag at L3 level
   *
   * @param storeId
   * @param itemPickupPoints
   * @param username
   * @param solrUsername
   */
  void validateAndUpdateProductCncActivatedFalse(String storeId, List<ItemPickupPoint> itemPickupPoints,
      String username, String solrUsername);

  void updateProductItemCncActivatedTrue(String storeId, String pickupPointCode, String username,
      List<OfflineItem> offlineItems);

  List<OfflineItem> findByMerchantCodeAndMerchantSku(String storeId, String merchantCode, String merchantSku);

  List<ProductAndItemsVO> findOfflineItemsByProductSkus(String storeId, String username,
      String requestId, List<String> productSkus) throws Exception;

  List<OfflineItem> findByItemSkusAndMarkForDeleteFalse(String storeId, Set<String> itemSkus);

  /**
   * find list of offline items by list of item skus and pickup point code
   *
   * @param storeId
   * @param itemSkus
   * @param pickupPointCode
   */
  List<OfflineItem> findByItemSkusAndPickupPointCodeAndMarkForDeleteFalse(String storeId, Set<String> itemSkus, String pickupPointCode);

  List<OfflineItem> findByItemSkusAndMarkForDeleteFalseWithLimit(String storeId, Set<String> itemSkus);

  /**
   * Fetch itemskus without pagination to avoid extra count query
   *
   * @param storeId
   * @param itemSkus
   * @param limit
   * @return
   */
  List<OfflineItem> findByItemSkusAndMarkForDeleteFalseWithLimitWithoutPagination(String storeId, Set<String> itemSkus,
      int limit);

  /**
   *
   * @param storeId
   * @param itemSku
   * @param pageable
   * @return
   */
  Page<OfflineItem> findByItemSkuAndMarkForDeleteFalse(String storeId, String itemSku, Pageable pageable);

  List<ItemPickupPointPriceVo> findMultipleMerchantsOfflineItemByItemSku(String storeId, String itemSku) throws Exception;

  List<OfflineItem> findByItemSku(String storeId, String itemSku);

  List<OfflineItem> findByPickupPointCode(String storeId, String pickupPointCode);

  OfflineItem findByItemSkuAndPickupPointCode(String storeId, String itemSku,
      String pickupPointCode);

  boolean isOfflineItem(String storeId, String uniqueId) throws Exception;

  /**
   * Update Offline Item Price by Item SKU
   *
   * @param mandatoryRequestParam must not be null
   * @param merchantCode must not be blank
   * @param updateOfflineItemPriceRequest must contains
            - itemSku must not be blank
            - listPrice must not be null and greater than minimum price from system parameter and offerPrice
            - offerPrice must not be null, greater than minimum price from system parameter, and less than listPrice
   */
  void updateOfflineItemPriceByItemSku(MandatoryRequestParam mandatoryRequestParam, String merchantCode, UpdateOfflineItemPriceRequest updateOfflineItemPriceRequest) throws Exception;

  /**
   * Find ItemPickupPointPriceVo list by merchantCode and itemSku
   *
   * @param storeId
   * @param merchantCode
   * @param itemSku
   * @return
   */
  List<ItemPickupPointPriceVo> findItemPickupPointByMerchantCodeAndItemSku(String storeId, String merchantCode, String itemSku);

  /**
   *
   *
   * @param storeId
   * @param offlineItemId
   * @return
   */
  ItemPickupPointPriceVo findItemPickupPointByOfflineItemId(String storeId, String offlineItemId);
}
