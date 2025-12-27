package com.gdn.x.product.service.api;

import java.util.Collection;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.Set;

import com.gdn.common.web.param.MandatoryRequestParam;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.x.product.domain.event.model.BlimartSubscriptionChangeRequest;
import com.gdn.x.product.domain.event.model.ItemPickupPointDataChangeEventModel;
import com.gdn.x.product.domain.event.model.PricingPwpPromoEvent;
import com.gdn.x.product.enums.ProductType;
import com.gdn.x.product.model.entity.Price;
import com.gdn.partners.product.pricing.streaming.model.promo.bundling.ItemInfo;
import com.gdn.x.product.model.vo.DeleteOfflineItemVO;
import com.gdn.x.product.model.vo.ItemPickupPointListingRequestVo;
import com.gdn.x.product.model.vo.ItemPickupPointPriceVo;

import com.gdn.x.product.rest.web.model.dto.AuditTrailDto;
import com.gdn.x.product.rest.web.model.request.AutoCreatePickupPointRequest;
import com.gdn.x.product.rest.web.model.request.CogsUpdateRequest;
import com.gdn.x.product.rest.web.model.request.CreateFbbPickupPointRequest;
import com.gdn.x.product.rest.web.model.request.DeleteOfflineItemRequest;
import com.gdn.x.product.rest.web.model.request.ItemPickupPointRequest;
import com.gdn.x.product.rest.web.model.response.AutoCreatePickupPointResponse;
import com.gdn.x.product.rest.web.model.response.CreateFbbPickupPointResponse;
import com.gdn.x.product.rest.web.model.response.CogsResponse;
import com.gdn.x.product.rest.web.model.response.ItemSkuAndPickupPointCodeResponse;
import com.gdn.x.product.rest.web.model.response.MinMaxItemPriceResponse;
import com.gdn.x.product.rest.web.model.response.ProductSkuPickupPointResponseV2;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;

import com.gdn.x.product.model.entity.Item;
import com.gdn.x.product.model.entity.ItemPickupPoint;
import com.gdn.x.product.model.entity.ItemViewConfig;
import com.gdn.x.product.rest.web.model.request.ItemPickupPointSummaryRequest;
import com.gdn.x.product.rest.web.model.request.ItemPickupPointViewConfigBaseRequest;
import com.gdn.x.product.rest.web.model.request.ItemRequestV2;
import com.gdn.x.product.rest.web.model.request.UpdateOfflineItemPriceRequest;
import com.gdn.x.product.rest.web.model.response.ItemSkuPickupPointCodeResponse;
import com.gdn.x.product.rest.web.model.response.ItemBasicDetailResponse;
import com.gdn.x.product.rest.web.model.response.ItemSkuPickupPointResponse;
import com.gdn.x.product.rest.web.model.response.PriceUpdatedInTimeRangeL5Response;
import com.gdn.x.product.rest.web.model.response.ProductSkuPickupPointResponse;

public interface ItemPickupPointService {

  /**
   * Find list of iten pick up point by storeId and itemSkus and pickup point code
   *
   * @param storeId
   * @param itemSkus
   * @param cncActivated
   * @param pickupPointCode
   * @return
   */
  List<ItemPickupPoint> findItemPickupPointByStoreIdAndItemSkusAndPickupPointCodeAndCncActivatedAndMarkForDeleteFalse(
      String storeId, Set<String> itemSkus, boolean cncActivated, String pickupPointCode);

  /**
   * update the item view config for l5
   *
   * @param storeId
   * @param username
   * @param doArchive
   * @param item
   * @param itemPickupPointList
   * @param isRejected
   */
  void updateItemViewConfigByItemSku(List<AuditTrailDto> auditTrailDtoList, String storeId, String username,
      boolean doArchive, Item item, List<ItemPickupPoint> itemPickupPointList, boolean isRejected);

  /**
   * @param itemPickupPoint
   * @param itemViewConfigSet
   */
  void updateItemViewConfigForExistingChannel(ItemPickupPoint itemPickupPoint, Set<ItemViewConfig> itemViewConfigSet);

  /**
   * Get ItemPickupPoint by itemSku and delivery flag
   *
   * @param storeId
   * @param itemSku
   * @return
   */
  ItemPickupPoint findByItemSkuAndDelivery(String storeId, String itemSku);

  /**
   * Get ItemPickupPoint list by itemSku list and delivery flag
   *
   * @param storeId
   * @param delivery
   * @param itemSkuList
   * @return
   */
  List<ItemPickupPoint> findByItemSkuInAndDelivery(String storeId, List<String> itemSkuList, boolean delivery);

  /**
   * Find page of ItemPickupPoint by storeId and itemSku
   *
   * @param storeId
   * @param itemSku
   * @param pageable
   * @return
   */
  Page<ItemPickupPoint> findByStoreIdAndItemSkuAndMarkForDeleteFalse(String storeId, String itemSku,
      Pageable pageable);

  /**
   * Find item pick up point by storeId, itemSku, cncActive and mfd
   *
   * @param storeId
   * @param itemSku
   * @param cncActive
   * @param markForDelete
   * @param pageable
   * @return
   */
  Page<ItemPickupPoint> findByStoreIdAndItemSkuAndCncActiveAndMarkForDelete(String storeId, String itemSku,
      boolean cncActive, boolean markForDelete, Pageable pageable);

  /**
   * Find ItemPickupPoint list by storeId and PickupPointCode
   *
   * @param storeId
   * @param pickupPointCode
   * @return
   */
  List<ItemPickupPoint> findByStoreIdAndPickupPointCode(String storeId, String pickupPointCode);

  /**
   * Find ItemPickupPoint list by storeId, PickupPointCode and markForDelete false
   *
   * @param storeId
   * @param pickupPointCode
   * @return
   */
  List<ItemPickupPoint> findByStoreIdAndPickupPointCodeAndMarkForDeleteFalse(String storeId, String pickupPointCode);

  /**
   * get count by storeId, PickupPointCode and markForDelete false
   *
   * @param storeId
   * @param pickupPointCode
   * @return
   */
  Long getCountByStoreIdAndPickupPointCodeAndMarkForDeleteFalse(String storeId, String pickupPointCode);

  /**
   * fetch by list of item skus
   * @param storeId
   * @param itemSkus
   * @param delivery
   * @return
   */
  List<ItemPickupPoint> findByItemSkusAndDelivery(String storeId, List<String> itemSkus, boolean delivery);

  /**
   * find one ItemPickupPoint By StoreId, PickupPointCode And MarkForDeleteFalse
   *
   * @param storeId
   * @param pickupPointCode
   * @return
   */
  ItemPickupPoint findOneByStoreIdAndPickupPointCodeAndMarkForDeleteFalse(String storeId, String pickupPointCode);

  /**
   * Get ItemPickupPoint by merchantCode, cncActivated and markForDelete
   *
   * @param storeId
   * @param merchantCode
   * @param cncActivated
   * @param markForDelete
   * @return
   */
  List<ItemPickupPoint> findByStoreIdAndMerchantCodeAndCncActiveAndMarkForDelete(String storeId, String merchantCode,
      boolean cncActivated, boolean markForDelete);

  /**
   * Get ItemPickupPoint by pickupPointCode, cncActivated, markForDelete
   *
   * @param storeId
   * @param pickupPointCode
   * @param cncActivated
   * @param markForDelete
   * @return
   */
  List<ItemPickupPoint> findByStoreIdAndPickupPointCodeAndCncActiveAndMarkForDelete(String storeId,
      String pickupPointCode, boolean cncActivated, boolean markForDelete);

  /**
   * Get ItemPickupPoint by pickupPointCode, cncActivated, markForDelete
   *
   * @param storeId
   * @param pickupPointCode
   * @param configFlagValue
   * @param markForDelete
   * @param channel
   * @return
   */
  List<ItemPickupPoint> findByStoreIdAndPickupPointCodeAndConfigFlagValuesAndMarkForDeleteAndChannel(
      String storeId, String pickupPointCode, boolean configFlagValue, boolean markForDelete,
      String channel);

  /**
   * Get ItemPickupPoint by merchantCode, cncActivated and markForDelete
   *
   * @param storeId
   * @param merchantCode
   * @param configFlagValue
   * @param markForDelete
   * @return
   */
  List<ItemPickupPoint> findByStoreIdAndMerchantCodeAndConfigFlagValueAndMarkForDelete(
      String storeId, String merchantCode, boolean configFlagValue, boolean markForDelete,
      String channel);

  /**
   * Save list of itemPickupPoint
   *
   * @param itemPickupPointList
   */
  List<ItemPickupPoint> saveItemPickupPoint(List<ItemPickupPoint> itemPickupPointList);


  /**
   * Save list of itemPickupPoint
   * @param itemPickupPointList
   * @param itemList
   * @return
   */
  List<ItemPickupPoint> saveItemPickupPoint(List<ItemPickupPoint> itemPickupPointList, List<Item> itemList);

  /**
   * Find itemPickupPoint by storeId itemSku cncActive and markForDelete
   *
   * @param storeId
   * @param itemSku
   * @param cncActive
   * @param markForDelete
   * @return
   */
  List<ItemPickupPoint> findByStoreIdAndItemSkuAndCncActiveAndMarkForDelete(String storeId,
      String itemSku, boolean cncActive, boolean markForDelete);

  /**
   * Find itemPickupPoint by storeId itemSku cnc buyable true and markForDelete false
   *
   * @param storeId
   * @param itemSku
   * @return
   */
  List<ItemPickupPoint> findByStoreIdAndItemSkuAndCncBuyableTrueAndMarkForDeleteFalse(String storeId, String itemSku);

  /**
   * Fetch all l5s
   *
   * @param storeId
   * @param itemSku
   */
  List<ItemPickupPoint> findByStoreIdAndItemSkuAndMarkForDeleteFalse(String storeId, String itemSku);

  /**
   *
   * @param storeId
   * @param itemSku
   * @return
   */
  List<ItemPickupPoint> findByStoreIdAndItemSkuAndMarkForDeleteFalseReadFromPrimary(String storeId,
      String itemSku);

  /**
   * Update discount price in L5 table
   *
   * @param itemPickupPoint
   */
  void updateDiscountPriceInItemPickupPoint(ItemPickupPoint itemPickupPoint);

  /**
   * Update discount price in L5 table
   *
   * @param itemPickupPoint
   */
  void updateDiscountPriceInItemPickupPointByItemSkuAndPickupPointCode(ItemPickupPoint itemPickupPoint);

  /**
   * Find L5 by merchant code and itemSku
   *
   * @param storeId
   * @param merchantCode
   * @param itemSku
   * @return
   */
  List<ItemPickupPointPriceVo> findByStoreIdAndMerchantCodeAndItemSku(String storeId, String merchantCode, String itemSku);

  /**
   * Find ItemPickupPointPriceVo list by item Skus
   *
   * @param storeId
   * @param itemSkusFromMultipleMerchants
   * @param pageable
   * @return
   */
  List<ItemPickupPointPriceVo> findItemPickupPointPriceVoByItemSkus(String storeId,
      Set<String> itemSkusFromMultipleMerchants, Pageable pageable);

  /**
   * Find ItemPickupPoints by storeId and L4 codes and CncActive flag
   *
   * @param storeId
   * @param itemSkus
   * @param cncActive
   * @return
   */

  /**
   *
   * @param storeId
   * @param productSku
   * @return
   */
  List<ItemPickupPoint> findItemPickupPointByProductSkus(String storeId,
      List<String> productSku);

  /**
   * Delete ItemPickupPoint By StoreId And ProductSkus
   *
   * @param storeId
   * @param productSkus
   * @return
   */
  List<ItemPickupPoint> deleteItemPickupPointByStoreIdAndProductSkus(String storeId, Set<String> productSkus);

  List<ItemPickupPoint> findItemPickupPointByStoreIdAndItemSkusAndCncActiveAndMarkForDeleteFalse(
      String storeId, Set<String> itemSkus, boolean cncActive);

  /**
   * fetch itemPickupPoint based on itemSku and pickupPointCode
   * @param storeId
   * @param itemSku
   * @param pickupPointCode
   * @return
   */
  ItemPickupPoint findByItemSkuAndPickupPointCode(String storeId, String itemSku, String pickupPointCode);

  /**
   *
   * @param storeId
   * @param itemSku
   * @param pickupPointCode
   * @return
   */
  ItemPickupPoint findByItemSkuAndPickupPointCodeReadFromPrimary(String storeId, String itemSku,
      String pickupPointCode);

  /**
   * Find itemPickupPoint by storeId itemSku and pickUpPointCode from Db
   *
   * @param storeId
   * @param itemSku
   * @param pickupPointCode
   * @return
   */
  ItemPickupPoint findByItemSkuAndPickupPointCodeFromDb(String storeId, String itemSku, String pickupPointCode);

  /**
   * fetch and update itemPickupPoint base don itemSku and merchantCode
   *
   * @param storeId
   * @param merchantCode
   * @return
   */
  List<ItemPickupPoint> updateItemPickupPointPriceByOfflineItem(String storeId, String merchantCode,
      UpdateOfflineItemPriceRequest updateOfflineItemPriceRequest);

  /**
   * Update specified field, after update succeed, the new data will be returned.
   *
   * @param storeId
   * @param itemSku
   * @param fieldName
   * @param value
   * @return
   */
  ItemPickupPoint updateFieldByItemSku(String storeId, String itemSku, String fieldName, Object value);

  /**
   *
   * Update specified field, after update succeed, the new data will be returned.
   *
   * @param storeId
   * @param itemSku
   * @param fieldName
   * @param value
   * @return
   */
  List<ItemPickupPoint> updateFieldsByItemSku(String storeId, String itemSku, String fieldName, Object value);

  /**
   * Fetch all ItemPickupPoint list for product sku and delivery flag
   *
   * @param storeId
   * @param productSku
   * @param delivery must not be blank
   */
  List<ItemPickupPoint> findByStoreIdAndProductSkuAndDelivery(String storeId,
      String productSku, boolean delivery);

  /**
   * Find ItemPickupPoint by itemSku and delivery
   *
   * @param storeId  must not be blank
   * @param itemSku  must not be blank
   * @param delivery must not be blank
   * @return ItemPickupPoint
   */
  ItemPickupPoint findByItemSkuAndDelivery(String storeId, String itemSku, boolean delivery);

  /**
   * Get L5s Created In Time Range
   *
   * @param storeId
   * @param startDate
   * @param endDate
   * @param page
   * @param size
   * @param requestId
   * @return
   */
  GdnRestListResponse<ItemSkuAndPickupPointCodeResponse> getL5sCreatedInTimeRange(String storeId, Date startDate, Date endDate,
      int page, int size, String requestId);

  /**
   * Getting L5s Price Updated In Time Range
   *
   * @param storeId
   * @param startDate
   * @param endDate
   * @param page
   * @param size
   * @param requestId
   * @return
   */
  GdnRestListResponse<PriceUpdatedInTimeRangeL5Response> getL5sPriceUpdatedInTimeRange(String storeId, Date startDate,
      Date endDate, int page, int size, String requestId);

  /**
   * Find ItemPickupPointPriceVo by offlineItemId
   *
   * @param storeId
   * @param offlineItemId
   * @return
   */
  ItemPickupPointPriceVo findByItemPickupPointPriceVoStoreIdAndOfflineItemIdAndMarkForDeleteFalse(String storeId,
      String offlineItemId);

  /**
   * Find ItemPickupPoint by unique id
   *
   * @param storeId
   * @param uniqueId
   * @return
   */
  ItemPickupPoint findByStoreIdAndOfflineItemIdAndMarkForDeleteFalse(String storeId, String uniqueId);

  /**
   * Save collection of ItemPickupPoint
   *
   * @param itemPickupPoint
   * @return
   */
  ItemPickupPoint saveItemPickupPoint(ItemPickupPoint itemPickupPoint);

  /**
   * Save L5 collection
   *
   * @param existingItemPickupPoints
   */
  void saveItemPickupPointCollection(List<ItemPickupPoint> existingItemPickupPoints);

  /**
   * Update mark
   *
   * @param storeId
   * @param businessPartnerCode
   * @return
   */
  List<ItemPickupPoint> findByStoreIdAndMerchantCodeAndMarkForDeleteFalse(String storeId, String businessPartnerCode);


  /**
   * Update MPD flag in item pickup point table
   *
   * @param storeId
   * @param itemSku
   * @param isPromoActive
   */
  boolean updateMerchantPromoDiscountFlag(String storeId, String itemSku, boolean isPromoActive);


  /**
   * Update promo bundling flag
   *
   * @param storeId
   * @param itemSkus
   * @param fieldName
   * @param delivery
   * @param value
   * @return
   */
  List<ItemPickupPoint> updateFieldByItemSkusAndDelivery(String storeId, Collection<String> itemSkus, String fieldName,
      boolean delivery, Object value);

  /**
   * Update promo bundling flag
   *
   * @param storeId
   * @param itemInfos
   * @param fieldName
   * @param value
   * @return
   */
  List<ItemPickupPoint> updateFieldByItemSkusAndPPCode(String storeId, List<ItemInfo> itemInfos, String fieldName,
       Object value);


  /**
   * Find one by productSku and mfd false
   *
   * @param storeId
   * @param itemSku
   * @return
   */
  ItemPickupPoint findOneByStoreIdAndProductSkuAndMarkForDeleteFalse(String storeId, String itemSku);

  /**
   * Get ItemPickupPoint by itemSku, pickup point, cnc active
   *
   * @param storeId
   * @param itemSku
   * @param pickupPointCode
   * @return ItemPickUpPoint
   */
  ItemPickupPoint findByStoreIdAndItemSkuAndPickupPointCodeAndCncActiveAndMarkForDeleteFalse(String storeId,
      String itemSku, String pickupPointCode, boolean CncActive);

  /**
   * Delete item pickup points
   *
   * @param storeId
   * @param merchantCode
   * @param username
   */
  void delete(String storeId, String merchantCode, List<DeleteOfflineItemRequest> deleteOfflineItemRequests, String username)
      throws Exception;

  /**
   * Bulk delete item pickup point with returning success and failure count of offline items
   * deletion
   *
   * @param storeId
   * @param merchantCode
   * @param offlineItemsToDelete
   */
  List<DeleteOfflineItemVO> bulkDelete(String storeId, String merchantCode,
      List<DeleteOfflineItemVO> offlineItemsToDelete, String username) throws Exception;

  ItemPickupPoint findOneByItemSkuAndCncActiveAndMarkForDeleteFalse(String storeId,String itemSku,
      boolean cncActivate);

  ItemPickupPoint findOneByItemSkuAndCncActiveAndPickupPointCodeNotInAndMarkForDeleteFalse(String storeId,
      String itemSku, boolean cncActive, Set<String> pickupPointCodes);

  /**
   * find ItemPickupPoint by itemSku,cnc item configs, markForDelete, pickupPoint not in the set
   *
   * @param storeId
   * @param pickupPointCodes
   * @param cncActivated
   * @param itemSku
   * @return
   */
  ItemPickupPoint findOneByItemSkuAndCncActiveConfigAndPickupPointCodeNotInAndMarkForDeleteFalse(
      String storeId, String itemSku, Set<String> pickupPointCodes, boolean cncActivated);

  /**
   * find any cnc active L5 from
   * @param storeId
   * @param itemSku
   * @param cncActivate
   * @return
   */
  ItemPickupPoint findOneByProductSkuAndCncActiveAndMarkForDeleteFalse(String storeId,String itemSku,
      boolean cncActivate);

  /**
   * find any cnc active L5 from
   * @param storeId
   * @param itemSku
   * @param cncActivate
   * @return
       */
  ItemPickupPoint findOneByProductSkuAndCncActiveAndPickupPointCodeNotInAndMarkForDeleteFalse(String storeId,String itemSku,
      boolean cncActivate, Set<String> pickupPointCodes);

  /**
   * find ItemPickupPoint by productSku,cnc item configs, markForDelete, pickupPoint not in the set
   *
   * @param storeId
   * @param pickupPointCodes
   * @param cncActivated
   * @param productSku
   * @return
   */
  ItemPickupPoint findOneByProductSkuAndCncActiveConfigAndPickupPointCodeNotInAndMarkForDeleteFalse(
      String storeId, String productSku, Set<String> pickupPointCodes, boolean cncActivated);

  /**
   * Update active promo bundlings
   *
   * @param storeId
   * @param itemSku
   * @param promoBundlingType
   * @param clearPromoBundlings
   * @return
   */
  ItemPickupPoint updateActivePromoBundling(String storeId, String itemSku, String promoBundlingType, boolean clearPromoBundlings);

  /**
   * Update active promo bundlings by pp code and item sku
   *
   * @param storeId
   * @param itemSku
   * @param promoBundlingType
   * @param clearPromoBundlings
   * @return
   */
  ItemPickupPoint updateActivePromoBundlingByItemSkuAndPPCode(String storeId, String itemSku, String promoBundlingType,
      boolean clearPromoBundlings, String ppCode);

  /**
   * fetch l5 based on offlineItemId
   *
   * @param storeId
   * @param offlineItemId
   */
  ItemPickupPoint findByStoreIdAndOfflineItemId(String storeId, String offlineItemId);


  /**
   * Fetch L5's based on L5 id's
   *
   * @param storeId
   * @param offlineItemIds
   * @return
   */
  List<ItemPickupPoint> findByStoreIdAndOfflineItemIds(String storeId, List<String> offlineItemIds);

  /**
   * fetch l5 based on pickupPointCode and itemSku
   *
   * @param storeId
   * @param pickupPointCode
   * @param itemSku
   */
  ItemPickupPoint findByStoreIdAndItemSkuAndPickupPointCodeAndMarkForDeleteFalse(String storeId, String itemSku, String pickupPointCode);

  /**
   *
   * @param storeId
   * @param itemSku
   * @param pickupPointCode
   * @return
   */
  ItemPickupPoint findByStoreIdAndItemSkuAndPickupPointCodeAndMarkForDeleteFalseReadFromPrimary(String storeId, String itemSku,
      String pickupPointCode);

  /**
   * Find L5 based on itemSku and list of pickupPointCodes
   *
   * @param storeId
   * @param itemSku
   * @param pickupPointCode
   * @return
   */
  List<ItemPickupPoint> findByStoreIdAndItemSkuAndPickupPointCodeInAndMarkForDeleteFalse(String storeId, String itemSku,
      List<String> pickupPointCode);

  /**
   * fetch delivery L5 by item sku
   *
   * @param storeId
   * @param itemSku
   */
  ItemPickupPoint findByStoreIdAndItemSkuAndDeliveryTrueAndMarkForDeleteFalse(String storeId, String itemSku);

  /**
   * fetch all L5 by product sku
   *
   * @param storeId
   * @param productSku
   * @return
   */
  List<ItemPickupPoint> findByStoreIdAndProductSku(String storeId, String productSku);

  /**
   *
   * @param storeId
   * @param productSku
   * @return
   */
  List<ItemPickupPoint> findByStoreIdAndProductSkuReadFromPrimary(String storeId, String productSku);

  /**
   * fetch all L5 by item sku
   *
   * @param storeId
   * @param itemSku
   * @return
   */
  List<ItemPickupPoint> findByStoreIdAndItemSku(String storeId, String itemSku);


  /**
   * Fetch all L5's by item skus and mfd false
   *
   * @param storeId
   * @param itemSkus
   * @return
   */
  List<ItemPickupPoint> findByStoreIdAndItemSkuInAndMarkForDeleteFalse(String storeId, List<String> itemSkus);


  /**
   *
   * @param storeId
   * @param itemSkus
   * @return
   */
  List<ItemPickupPoint> findByStoreIdAndItemSkuInAndMarkForDeleteFalseReadFromPrimary(String storeId, List<String> itemSkus);

  /**
   * Fetch by offlineItemId list and markForDelete false
   *
   * @param storeId
   * @param offlineItemIdList
   * @return
   */
  List<ItemPickupPoint> findByStoreIdAndOfflineItemIdListAndMarkForDeleteFalse(String storeId,
    List<String> offlineItemIdList);

  /**
   * Get Item PP by productSku and PP code
   *
   * @param storeId
   * @param productSku
   * @param ppcode
   * @return
   */
  List<ItemPickupPoint> findItemPickupPointsByProductSkuAndPPcode(String storeId, String productSku, String ppcode);

  /**
   * Get active promo bundling by itemSkus
   *
   * @param storeId
   * @param itemSkus
   * @return
   */
  Set<String> getActivePromoBundlingByItemSkus(String storeId, List<String> itemSkus);

  /**
   * @param itemToSet
   * @param updatedPrices
   * @param username
   * @return
   */
  boolean validatePriceChangeAndSet(ItemPickupPoint itemToSet, Set<Price> updatedPrices,
    String username);

  /**
   *  Publish event on update of ItemPickupPoint change
   * @param itemPickupPoint
   */
  void publishItemPickupPointChangeEvent(ItemPickupPoint itemPickupPoint);

  /**
   * publish itemPickupPointData change event with pureCncStatusChange populated
   *
   * @param itemPickupPointDataChangeEventModel
   * @param eventToBlackListedSellersMap
   */
  void publishItemPickupPointDataChangeEventWithPureCncStatusChange(
    ItemPickupPointDataChangeEventModel itemPickupPointDataChangeEventModel,
    Map<String, Set<String>> eventToBlackListedSellersMap);


  /**
   * get active promo bundling by itemSkus
   * @param storeId
   * @param itemSkus
   * @param pickupPointCode
   * @return
   */
  Set<String> getActivePromoBundlingByItemSkusAndPickupPointCode(String storeId, List<String> itemSkus, String pickupPointCode);

  /**
   * Get item pickup point by storeId and productSku and ppCode
   *
   * @param storeId
   * @param productSku
   * @param ppcode
   * @param showDeleted
   * @return
   */
  List<ItemPickupPoint> findItemPickupPointsByProductSkuAndPPCodeWithShowDeleted(String storeId, String productSku,
      String ppcode, boolean showDeleted);

  /**
   * Find list of L5s by productSku and pickup point codes
   *
   * @param storeId
   * @param productSku
   * @param pickupPointCodes
   * @return
   */
  List<ItemPickupPoint> findItemPickupPointsByProductSkuAndPickupPointCodes(String storeId, String productSku,
      List<String> pickupPointCodes);

  /**
   *
   * @param storeId
   * @param productSku
   * @param pickupPointCodes
   * @return
   */
  List<ItemPickupPoint> findItemPickupPointsByProductSkuAndPickupPointCodesReadFromPrimary(String storeId, String productSku,
      List<String> pickupPointCodes);


  /**
   * Fetch item Pickup points by product sku
   *
   * @param storeId
   * @param productSku
   * @return
   */
  List<ItemPickupPoint> getItemPickupPointsByProductSkuAndMarkForDeleteFalse(String storeId, String productSku);


  /**
   *
   * @param storeId
   * @param productSku
   * @return
   */
  List<ItemPickupPoint> getItemPickupPointsByProductSkuAndMarkForDeleteFalseReadFromPrimary(String storeId, String productSku);

  /**
   * Get ItemPickupPoint list by itemSku list and delivery flag
   *
   * @param storeId
   * @param delivery
   * @param itemSkuList
   * @return
   */
  List<ItemSkuPickupPointCodeResponse> findPickUpPointCodeByItemSkuInAndDelivery(String storeId,
      List<String> itemSkuList, boolean delivery);

  /**
   * updateItemViewConfigWithItemStatusInItemPickupPoint
   *
   * @param storeId
   * @param userName
   * @param requestId
   * @param productSku
   * @param itemPickupPointViewConfigBaseRequest
   */
  void updateItemPickupPointViewConfigWithProductStatus(String storeId, String userName, String requestId,
      String productSku, ItemPickupPointViewConfigBaseRequest itemPickupPointViewConfigBaseRequest) throws Exception;


  /**
   * get itemPickupPoint for summary listing using filters
   * @param storeId
   * @param page
   * @param size
   * @param itemPickupPointSummaryRequest
   * @return
   */
  Page<ItemPickupPoint> findItemPickupPointForSummaryListing(String storeId, int page, int size,
      ItemPickupPointSummaryRequest itemPickupPointSummaryRequest, Set<String> itemSkus);

  /**
   * get itemPickupPoint by itemSkus
   *
   * @param storeId
   * @param itemRequestV2
   * @param page
   * @param pageSize
   * @param excludeDistributionPickupPoint
   * @return
   */
  Page<ItemPickupPoint> findItemPickupPointByItemSkus(String storeId, ItemRequestV2 itemRequestV2, int page, int pageSize,
      boolean excludeDistributionPickupPoint);

  /**
   * Get item pickup points by itemSku and pickupPointCode
   *
   * @param storeId
   * @param itemPickupPointRequests
   * @param inAllProducts
   * @return
   */
  List<ItemPickupPoint> fetchItemPickupPointsByItemSkuAndPickupPointCode(String storeId,
      List<ItemPickupPointRequest> itemPickupPointRequests, boolean inAllProducts);

  /**
   * method to publish list of item SKU and PickupPoint code to AGP
   *
   * @param itemPickupPointRequestList must not be empty
   * @param storeId                    must not be blank
   * @param republishToAgp
   */
  void republishItemPickupPointToAgp(List<ItemPickupPointRequest> itemPickupPointRequestList,
    String storeId, boolean republishToAgp);

  /**
   * get L5's by productSkus and pickupPoint codes
   *
   * @param storeId
   * @param productSku
   * @param pickupPointCodes
   * @return
   */
  List<ItemPickupPoint> findItemPickupPointByProductSkusAndPickupPointCodes(String storeId, List<String> productSku,
      List<String> pickupPointCodes);

  /**
   *
   * @param storeId
   * @param productSku
   * @param itemSku
   * @param page
   * @param pageSize
   * @return
   */
  Page<ItemPickupPoint> findItemPickupPointByProductSkusOrItemSkus(String storeId,
      List<String> productSku, List<String> itemSku, int page, int pageSize);

  /**
   *
   * @param storeId
   * @param productSku
   * @param itemSku
   * @param page
   * @param pageSize
   * @return
   */
  Page<ItemPickupPoint> findAllItemPickupPointByProductSkusOrItemSkus(String storeId, List<String> productSku,
      List<String> itemSku, int page, int pageSize);

  /**
   * @param storeId
   * @param productSku
   * @param itemSku
   * @param fbbActivated
   * @param page
   * @param pageSize
   * @return
   */
  Page<ItemPickupPoint> findItemPickupPointByProductSkusOrItemSkusForFbb(String storeId,
      List<String> productSku, List<String> itemSku, boolean fbbActivated, int page, int pageSize);


  /**
   * fetch l5 listing based on product sku or item sku
   * @param storeId
   * @param productSku
   * @param itemSku
   * @param page
   * @param pageSize
   * @return
   */
  Page<ItemPickupPoint> findItemPickupPointByProductSkuOrItemSku(String storeId, String productSku, String itemSku, int page, int pageSize);

  /**
   * fetch l5 listing based on product sku and filters
   * @param storeId
   * @param page
   * @param size
   * @param itemPickupPointListingRequestVo
   * @return
   */
  Page<ItemPickupPoint> getItemPickupPointListingByProductSku(String storeId, int page, int size, ItemPickupPointListingRequestVo itemPickupPointListingRequestVo);

  /**
   * Find count of Active L5s for a given L4
   *
   * @param storeId
   * @param itemSku
   * @return
   */
  Long findCountByStoreIdAndItemSkuAndMarkForDeleteFalse(String storeId, String itemSku);


  /**
   *
   * @param storeId
   * @param itemSku
   * @return
   */
  Long findCountByStoreIdAndItemSkuAndMarkForDeleteFalseReadFromPrimary(String storeId, String itemSku);

  /**
   * Find fbb true pickupPoints by storeId and itemSkus and markForDelete false
   *
   * @param storeId
   * @param itemSkus
   * @param fbbActivated
   * @return
   */
  List<ItemPickupPoint> findByStoreIdAndItemSkuInAndMarkForDeleteFalseAndFbbActivated(String storeId,
      Set<String> itemSkus, boolean fbbActivated);

  /**
   *
   * @param storeId
   * @param itemSkus
   * @param fbbActivated
   * @return
   */
  List<ItemPickupPoint> findByStoreIdAndItemSkuInAndMarkForDeleteFalseAndFbbActivatedReadFromPrimary(String storeId,
      Set<String> itemSkus, boolean fbbActivated);

  /**
   * Find count of Active L5s for a given L3
   * @param storeId
   * @param productSku
   * @return
   */
  Long findCountByStoreIdAndProductSkuAndMarkForDeleteFalse(String storeId, String productSku);

  /**
   * Find count of Active CNC_FLAG_UPDATE L5s for a given L4
   *
   * @param storeId
   * @param itemSku
   * @param cncActive
   * @return
   */
  Long findCountByStoreIdAndItemSkuAndMarkForDeleteFalseAndCncActive(String storeId, String itemSku, boolean cncActive);

  /**
   * Find count of Non-offline CNC channel L5s by L4
   *
   * @param storeId
   * @param itemSku
   * @return
   */
  Long findCountByStoreIdAndItemSkuAndMarkForDeleteFalseAndCncViewConfig(String storeId, String itemSku, String cncChannel);


  /**
   * Exists Any L5 with cnc
   * @param storeId
   * @param itemSku
   * @param cncChannel
   * @return
   */
  Boolean existsCountByStoreIdAndItemSkuAndMarkForDeleteFalseAndCncBuyable(String storeId,
      String itemSku, String cncChannel);

  /**
   *
   * @param storeId
   * @param itemSku
   * @param cncActive
   * @return
   */
  Long findCountByStoreIdAndItemSkuAndMarkForDeleteFalseAndCncActiveReadFromPrimary(String storeId, String itemSku, boolean cncActive);

  /**
   * Find count of FBB active itemPickupPoints by productSku
   *
   * @param storeId
   * @param productSku
   * @param fbbActivated
   * @return
   */
  Long findCountByStoreIdAndProductSkuAndMarkForDeleteFalseAndFbbActivated(String storeId, String productSku, boolean fbbActivated);

  /**
   * get itemPikcupPoint by itemSku and pickupPointcode or delivery true
   * @param storeId
   * @param itemSkus
   * @param pickupPointCode
   * @return
   */
  List<ItemPickupPoint> getItemPickupPointByItemSkuInAndPickupPointCode(String storeId, Set<String> itemSkus, String pickupPointCode);

  /**
   * get itemPickupPoint by productSkus
   * @param storeId
   * @param productSkuList
   * @return
   */
  Map<String, List<ItemPickupPoint>> getItemPickupPointByProductSkuList(String storeId, List<String> productSkuList);

  /**
   * get itemPickupPoint by productSkus and delivery true
   * @param storeId
   * @param productSkuList
   * @return
   */
  Map<String, List<ItemPickupPoint>> getItemPickupPointByProductSkuListAndDeliveryTrue(String storeId, List<String> productSkuList);

  /**
   * get itemPickupPoint by itemSkuList and delivery true
   * @param storeId
   * @param itemSkuList
   * @return
   */
  List<ItemPickupPoint> getItemPickupPointByItemSkuInAndDeliveryTrue(String storeId, List<String> itemSkuList);

  /**
   * get itemPickupPoint by itemSkuList and delivery true
   * @param storeId
   * @param productSku
   * @param page
   * @param size
   * @return
   */
  Page<ItemPickupPoint> getItemPickupPointByStoreIdAndProductSkuInAndDeliveryTrue(String storeId,
    String productSku, int page, int size);

  /**
   * get basic item details
   * @param storeId
   * @param page
   * @param size
   * @param itemSku
   * @return
   * @throws Exception
   */
  ItemBasicDetailResponse getBasicItemDetails(String storeId, int page, int size, String itemSku);

  /**
   * soft delete item pickup point for a itemSku
   */
  void deleteItemPickupPoints(String storeId, String itemSku);

  /**
   * get online or cnc pickup based on product sku and republish to agp
   * @param storeId
   * @param productSkuList
   * @param republish
   */
  List<ProductSkuPickupPointResponse> getL5BasedOnProductSkuListAndOnlineOrCncFlagAndRepublishToAgp(String storeId,
      List<String> productSkuList, boolean republish);

  /**
   * get online or cnc pickup based on item sku and republish to agp
   * @param storeId
   * @param productSkuList
   * @param republish
   */
  List<ItemSkuPickupPointResponse> getL5BasedOnItemSkuListAndOnlineOrCncFlagAndRepublishToAgp(String storeId,
      List<String> productSkuList, boolean republish);

  /**
   * Get all active L5s for merchant code and pp code
   *
   * @param storeId
   * @param merchantCode
   * @param pickupPointCode
   * @return
   */
  Page<ProductSkuPickupPointResponseV2> getProductSkuListByBusinessPartnerAndPickupPointCode(
    String storeId, String merchantCode, String pickupPointCode, int page, int pageSize);

  /**
   * Get min and max price at L3
   *
   * @param storeId storeId
   * @param productCode productCode
   * @return minMaxPrice response
   */
  MinMaxItemPriceResponse getMinAndMaxOfferPrice(String storeId, String productCode);

  /**
   * Create Fbb item pickup point
   *
   * @param storeId
   * @param createFbbPickupPointRequest
   * @param item
   * @param createFbbPickupPointResponse1
   * @return
   */
  CreateFbbPickupPointResponse createFbbPickupPoint(String storeId,
      CreateFbbPickupPointRequest createFbbPickupPointRequest, Item item, CreateFbbPickupPointResponse createFbbPickupPointResponse1);

  /**
   *
   * @param storeId
   * @param itemSku
   * @return
   */
  ItemPickupPoint findByStoreIdAndItemSkuAndFbbActivatedTrueAndMarkForDeleteFalse(String storeId,
    String itemSku);


  /**
   *
   * @param itemPickupPointList
   * @return
   */
  List<ItemPickupPoint> updateFbbFlag(List<ItemPickupPoint> itemPickupPointList);


  /**
   * get pickup points which are added to other variant
   * @param productSku
   * @param offlineItemIds
   * @return
   */
  Set<String> pickupPointsAddedToOtherVariants(String productSku, Set<String> offlineItemIds);

  /**
   * @param storeId    must not be null
   * @param productSku must not be null
   * @param cncActivated      must not be null
   * @param page       must not be null
   * @param pageSize   must not be null
   * @return
   */
  Page<ItemPickupPoint> findItemPickupPointByProductSkus(String storeId, List<String> productSku,
      Boolean cncActivated, Integer page, Integer pageSize);

  /**
   * auto create L5 with item sku and pp code
   *
   * @param autoCreatePickupPointRequest
   * @param mandatoryRequestParam
   * @param item
   * @param autoCreatePickupPointResponse
   * @param productType
   * @return
   */
  AutoCreatePickupPointResponse autoCreateL5(AutoCreatePickupPointRequest autoCreatePickupPointRequest,
      MandatoryRequestParam mandatoryRequestParam, Item item,
      AutoCreatePickupPointResponse autoCreatePickupPointResponse, ProductType productType);

  /**
   * Find fbb true online L5s
   *
   * @param storeId
   * @param itemSkus
   * @return
   */
  List<ItemSkuPickupPointCodeResponse> findFbbTrueOnlinePickupPointsAndItemSkusIn(String storeId,
      List<String> itemSkus);


  /**
   * delete l5 from x-inventory
   * @param itemPickupPointToBeDeleted
   */
  void deleteItemPickupPointsFromInventory(List<ItemPickupPoint> itemPickupPointToBeDeleted);

  /**
   * get item sku and fbb map using item sku list
   * @param storeId
   * @param itemSkusList
   * @return
   */
  Map<String,Boolean> getItemSkuFbbMap(String storeId, Set<String> itemSkusList);

  /**
   * fetch one L5 for each L4
   * @param storeId String
   * @param itemSkus List of String
   * @return List of String
   */
  List<ItemPickupPoint> findOneForEachItemSkuIn(String storeId, List<String> itemSkus);

  /**
   * Update subscribable flag
   *
   * @param blimartSubscriptionChangeRequest
   */
  void updateSubscriptionFlag(BlimartSubscriptionChangeRequest blimartSubscriptionChangeRequest);

  /**
   * fetch Basic Details By ItemSku And PickupPointCode List
   * @param storeId String
   * @param itemPickupPointRequest List of item Sku and pickupPointCode
   * @return List of view config response
   */
  List<ItemPickupPoint> fetchBasicDetailsByItemSkuAndPickupPointCodeList(String storeId,
    List<ItemPickupPointRequest> itemPickupPointRequest);


  /**
   * fetch L5 based on offlineItemIds and cncBuyable
   *
   * @param storeId
   * @param offlineItemIds
   * @param buyable
   * @param channel
   * @param limit
   * @return
   */
  List<ItemPickupPoint> findByStoreIdAndOfflineItemIdsAndBuyableAndMarkForDeleteFalseAndLimit(
      String storeId, Set<String> offlineItemIds, boolean buyable, String channel, int limit);

  /**
   * fetch L5 based on offlineItemIds and cncBuyable
   *
   * @param storeId
   * @param productSku
   * @param cncActivated
   * @return
   */

  ItemPickupPoint findFirstByProductSkuAndCncBuyableConfigAndMarkForDeleteFalse(String storeId,
      String productSku, boolean cncActivated);

  /**
   * Update pwp flags by itemSku and pickupPointCode
   *
   * @param pricingPwpPromoEvent
   */
  void updatePwpFlagsByItemSkuAndPickupPointCode(PricingPwpPromoEvent pricingPwpPromoEvent);

  /**
   * COGS update request
   *
   * @param storeId
   * @param cogsUpdateRequests
   */
  void updateInsuredAmountInItemPickupPoint(String storeId, List<CogsUpdateRequest> cogsUpdateRequests);

  /**
   * Get COGS data for a product with pagination
   *
   * @param storeId
   * @param productSku
   * @param page
   * @param size
   * @return
   */
  Page<CogsResponse> getCogsData(String storeId, String productSku, int page, int size);

  /**
   * Get Distribution true L5s
   *
   * @param storeId
   * @param itemSkus
   * @return
   */
  List<ItemPickupPoint> findByStoreIdAndItemSkuInAndDistributionTrueAndMarkForDeleteFalse(String storeId,
      List<String> itemSkus);
}
