package com.gdn.x.product.dao.api;

import java.util.Collection;
import java.util.List;
import java.util.Set;

import org.springframework.data.domain.Page;

import com.gdn.x.product.model.entity.ItemPickupPoint;
import com.gdn.x.product.model.vo.FieldUpdateRequestVo;
import com.gdn.x.product.model.vo.ItemPickupPointListingRequestVo;
import com.gdn.x.product.model.vo.ItemPickupPointSummaryRequestVo;
import com.gdn.x.product.model.vo.ItemPickupPointRequestVo;

import org.springframework.data.domain.Sort;

public interface ItemPickupPointRepositoryCustom {

  List<ItemPickupPoint> updateDiscountPriceInItemPickupPoint(ItemPickupPoint itemPickupPoint);

  List<ItemPickupPoint> updateDiscountPriceInItemPickupPointByItemSkuAndPickupPointCode(ItemPickupPoint itemPickupPoint);

  List<ItemPickupPoint> updateMerchantPromoDiscountFlag(String storeId, String itemSku, boolean isPromoActive);

  List<ItemPickupPoint> updateFieldByItemSkusAndDelivery(String storeId, Collection<String> itemSkus, String fieldName,
      boolean delivery, Object value);


  List<ItemPickupPoint> updateFieldByItemSkusAndItemInfos(String storeId, List<ItemPickupPointRequestVo> itemSkus, String fieldName,
      Object value);

  /**
   * @param storeId
   * @param itemSku
   * @param fieldName
   * @param value
   * @return
   */
  ItemPickupPoint updateFieldByItemSku(String storeId, String itemSku, String fieldName, Object value);

  /**
   * updating feilds in item pickup point by item sku
   *
   * @param storeId
   * @param itemSku
   * @param fieldName
   * @param value
   * @return
   */
  List<ItemPickupPoint> updateFieldsByItemSku(String storeId, String itemSku, String fieldName, Object value);

  Set<String> getActivePromoBundlingsByItemSkus(String storeId, List<String> itemSkus);

  /**
   * get active promo bundling by item sku and mark for delete false
   *
   * @param storeId
   * @param itemSkus
   * @param pickupPointCode
   * @return
   */
  Set<String> getActivePromoBundlingsByItemSkusAndMarkForDeleteFalse(String storeId, List<String> itemSkus, String pickupPointCode);

  /**
   *
   * @param offlineItemIds
   * @param listPrice
   * @param offerPrice
   */
  void updatePriceByOfflineItemIds(List<String> offlineItemIds, double listPrice,
    double offerPrice);

  /**
   * fetch itemPickupPoints using filters
   *
   * @param storeId
   * @param itemPickupPointSummaryRequestVo
   * @return
   */
  Page<ItemPickupPoint> getItemPickupPointListing(String storeId, int page, int size,
      ItemPickupPointSummaryRequestVo itemPickupPointSummaryRequestVo);

  /**
   * fetch item pickup points by item sku and pickup point code
   *
   * @param storeId
   * @param itemPickupPointRequestVoList
   * @param inAllProducts
   * @return
   */
  List<ItemPickupPoint> fetchItemPickupPointsByItemSkuAndPickupPointCode(String storeId, List<ItemPickupPointRequestVo> itemPickupPointRequestVoList,
      boolean inAllProducts);

  /**
   * fetch l5 listing based on product sku and filters
   * @param storeId
   * @param page
   * @param size
   * @param itemPickupPointListingRequestVo
   * @return
   */
  Page<ItemPickupPoint> getItemPickupPointListingByProductSku(String storeId, int page, int size,
      ItemPickupPointListingRequestVo itemPickupPointListingRequestVo);


  /**
   * update multiple fields by itemSku and pickupPointCode
   *
   * @param storeId
   * @param username
   * @param itemPickupPointRequestVoList
   * @param fieldUpdateRequestVoList
   */
  void updateFieldsByItemSkuAndPickupPointCodes(String storeId, String username,
      List<ItemPickupPointRequestVo> itemPickupPointRequestVoList, List<FieldUpdateRequestVo> fieldUpdateRequestVoList);

  /**
   * delete itemPickupPoints by itemSku
   * @param storeId
   * @param itemSku
   */
  void deleteItemPickupPointByItemSku(String storeId, String itemSku);

  /**
   * get itemPickupPoints by productSkus excluding the given ids
   * @param storeId
   * @param productSku
   * @param ids
   * @return
   */
  List<ItemPickupPoint> getItemPickupPointsByStoreIdAndProductSkuAndMarkForDeleteFalseAndIdNotIn(String storeId,
      String productSku, Set<String> ids);

  /**
   * get any online or cnc L5 for a given
   * @param storeId
   * @param productSku
   * @param itemSku
   * @param cncForWarehouseFeatureSwitch
   * @return
   */
  ItemPickupPoint findL5BasedOnSkuAndOnlineOrCncFlag(String storeId, String productSku,
      String itemSku, boolean cncForWarehouseFeatureSwitch);

  /**
   * Update fbb flag at L5 level
   *
   * @param storeId         non null store id
   * @param productSku      product sku
   * @param pickupPointCode pp code to update
   * @param fbbActivated    value of the fbb flag to update
   */
  List<ItemPickupPoint> updateFbbFlagByProductSkuAndPickupPointCode(String storeId,
    String productSku, String pickupPointCode, boolean fbbActivated);

  /**
   * Find fbb true online L5s for given set of itemSkus
   *
   * @param storeId
   * @param itemSkus
   * @return
   */
  List<ItemPickupPoint> findFbbTrueOnlinePickupPointsAndItemSkusIn(String storeId, List<String> itemSkus);


  List<ItemPickupPoint> findByStoreIdAndItemSkuInAndMarkForDeleteFalse(String storeId, List<String> itemSku,
      boolean primaryReadEnabled);

  ItemPickupPoint findByStoreIdAndItemSkuAndPickupPointCode(String storeId, String itemSku,
      String pickupPointCode, boolean primaryReadEnabled);

  List<ItemPickupPoint> findByStoreIdAndItemSkuAndMarkForDeleteFalse(String storeId, String itemSku, boolean primaryReadEnabled);

  List<ItemPickupPoint> findByStoreIdAndItemSkuAndSubscribableAndMarkForDeleteFalse(String storeId, String itemSku,
      boolean subscribable, boolean primaryReadEnabled);

  List<ItemPickupPoint> findByStoreIdAndItemSkuInAndFbbActivatedAndMarkForDeleteFalse(String storeId,
      Collection<String> itemSku, boolean fbbActivated, boolean primaryReadEnabled);

  ItemPickupPoint findByStoreIdAndItemSkuAndPickupPointCodeAndMarkForDeleteFalse(String storeId, String itemSku,
      String pickupPointCode, boolean primaryReadEnabled);

  List<ItemPickupPoint> findByStoreIdAndProductSku(String storeId, String productSku, boolean primaryReadEnabled);

  List<ItemPickupPoint> findByStoreIdAndProductSkuAndPickupPointCodeInAndMarkForDeleteFalse(String storeId,
      String productSku, Collection<String> pickupPointCode, boolean primaryReadEnabled);

  List<ItemPickupPoint> findByStoreIdAndProductSkuAndMarkForDeleteFalse(String storeId,
      String productSku, boolean primaryReadEnabled);

  Long countByStoreIdAndItemSkuAndMarkForDeleteFalse(String storeId, String itemSku, boolean primaryReadEnabled);

  Long countByStoreIdAndItemSkuAndMarkForDeleteFalseAndCncActive(String storeId, String itemSku, boolean cncActive, boolean primaryReadEnabled);

  List<ItemPickupPoint> fetchBasicDataByItemSkuAndPickupPointCode(String storeId,
    List<ItemPickupPointRequestVo> itemPickupPointRequestVoSubList);

  ItemPickupPoint findFirstByItemSkuAndCncBuyableConfigAndPickupPointCodeNotInAndMarkForDeleteFalse(
      String storeId, String itemSku, Set<String> pickupPointCodes, boolean cncActivated);

  ItemPickupPoint findFirstByProductSkuAndCncBuyableConfigAndPickupPointCodeNotInAndMarkForDeleteFalse(
      String storeId, String productSku, Set<String> pickupPointCodes, boolean cncActivated);

  List<ItemPickupPoint> findByStoreIdAndPickupPointCodeAndConfigFlagValueAndMarkForDeleteAndChannel(
      String storeId, String pickupPointCode, boolean configFlagValue, boolean markForDelete,
      String channel);

  Long countByStoreIdAndItemSkuAndMarkForDeleteFalseAndCncViewConfig(String storeId, String itemSku, String cncChannel,
      boolean readFromPrimary);

  boolean existsByStoreIdAndItemSkuAndMarkForDeleteFalseAndCncBuyable(String storeId, String itemSku,
      String cncChannel);

  List<ItemPickupPoint> findByStoreIdAndMerchantCodeAndConfigFlagValueAndMarkForDeleteAndChannel(
      String storeId, String merchantCode, boolean configFlagValue, boolean markForDelete,
      String channel);

  List<ItemPickupPoint> findByStoreIdAndItemSkuAndMarkForDeleteFalseAndCncBuyable(String storeId, String itemSku,
      String cncChannel);

  List<ItemPickupPoint> findByStoreIdAndOfflineItemIdsAndBuyableAndMarkForDeleteFalseAndLimit(
      String storeId, Set<String> offlineItemIds, boolean buyable, String channel, int limit);

  Page<ItemPickupPoint> findByStoreIdAndProductSkuAndMarkForDeleteFalseAndCncBuyable(String storeId,
      List<String> productSku, String cncChannel, int page, int size, Sort sort);

  ItemPickupPoint findFirstByProductSkuAndCncBuyableConfigAndMarkForDeleteFalse(String storeId,
      String productSku, boolean cncActivated);
}
