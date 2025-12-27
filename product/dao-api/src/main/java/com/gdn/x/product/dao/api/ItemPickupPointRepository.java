package com.gdn.x.product.dao.api;

import java.util.Date;
import java.util.List;
import java.util.Set;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.mongodb.repository.MongoRepository;
import org.springframework.data.mongodb.repository.Query;
import org.springframework.stereotype.Repository;

import com.gdn.x.product.model.entity.ItemPickupPoint;

@Repository
public interface ItemPickupPointRepository extends MongoRepository<ItemPickupPoint, String>, ItemPickupPointRepositoryCustom {

  ItemPickupPoint findByStoreIdAndItemSkuAndFbbActivatedTrueAndMarkForDeleteFalse(String storeId, String itemSku);

  List<ItemPickupPoint> findByStoreIdAndMerchantCodeAndMarkForDeleteFalse(String storeId,
    String businessPartnerCode);

  List<ItemPickupPoint> findByStoreIdAndItemSku(String storeId, String itemSku);

  List<ItemPickupPoint> findByStoreIdAndItemSkuInAndDeliveryTrueAndMarkForDeleteFalse(String storeId, List<String> itemSku);

  ItemPickupPoint findByStoreIdAndItemSkuAndDeliveryTrueAndMarkForDeleteFalse(String storeId, String itemSku);

  List<ItemPickupPoint> findByStoreIdAndItemSkuInAndDeliveryAndMarkForDeleteFalse(String storeId, List<String> itemskus,
      boolean delivery);

  ItemPickupPoint findFirstByStoreIdAndPickupPointCodeAndMarkForDeleteFalse(String storeId, String pickupPointCode);

  ItemPickupPoint findFirstByStoreIdAndItemSkuAndFbbActivatedTrueAndMarkForDeleteFalse(String storeId,
      String itemSku);

  List<ItemPickupPoint> findItemPickupPointByStoreIdAndItemSkuAndPickupPointCodeAndAndCncActiveAndMarkForDeleteFalse(
      String storeId, Set<String> itemSkus, boolean cncActive, String pickupPointCode);

  Page<ItemPickupPoint> findByStoreIdAndItemSkuAndMarkForDeleteFalse(String storeId, String itemSku, Pageable pageable);

  Page<ItemPickupPoint> findByStoreIdAndCreatedDateBetweenAndMarkForDeleteFalseAndForceReviewFalse(String storeId,
      Date startDate, Date endDate, Pageable pageable);

  Page<ItemPickupPoint> findByStoreIdAndPriceUpdatedDateBetweenAndMarkForDeleteFalse(String storeId, Date startDate,
      Date endDate, Pageable pageable);

  Page<ItemPickupPoint> findByStoreIdAndProductSkuAndMarkForDeleteFalse(String storeId, String productSku, Pageable pageable);

  List<ItemPickupPoint> findByStoreIdAndMerchantCodeAndCncActiveAndMarkForDelete(String storeId, String merchantCode,
      boolean cncActive, boolean markForDelete);

  List<ItemPickupPoint> findByStoreIdAndPickupPointCodeAndCncActiveAndMarkForDelete(String storeId,
      String pickupPointCode, boolean cncActive, boolean markForDelete);

  List<ItemPickupPoint> findByStoreIdAndItemSkuAndCncActiveAndMarkForDelete(String storeId,
      String itemSku, boolean cncActive, boolean markForDelete);

  Page<ItemPickupPoint> findByStoreIdAndItemSkuAndCncActiveAndMarkForDelete(String storeId,
      String itemSku, boolean cncActive, boolean markForDelete, Pageable pageable);

  List<ItemPickupPoint> findByStoreIdAndItemSkuAndMerchantCodeAndCncActiveTrueAndMarkForDeleteFalse(String storeId, String ItemSku, String merchantCode);

  /**
   * Find ItemPickupPoint by itemSku and pickup point code
   *
   * @param storeId must not be blank
   * @param itemSku must not be blank
   * @param pickupPointCode must not be blank
   * @return ItemPickupPoint
   */
  ItemPickupPoint findByItemSkuAndPickupPointCodeAndMarkForDeleteFalse(String storeId, String itemSku,
    String pickupPointCode);

  /**
   * Find ItemPickupPoint by itemSku and delivery
   *
   * @param storeId  must not be blank
   * @param itemSku  must not be blank
   * @param delivery must not be blank
   * @return ItemPickupPoint
   */
  ItemPickupPoint findByStoreIdAndItemSkuAndDeliveryAndMarkForDeleteFalse(String storeId,
    String itemSku, boolean delivery);

  ItemPickupPoint findByStoreIdAndOfflineItemIdAndCncActiveTrueAndMarkForDeleteFalse(String storeId, String offlineItemId);

  List<ItemPickupPoint> findByStoreIdAndOfflineItemIdInAndMarkForDeleteFalse(String storeId,
      List<String> offlineItemId);

  List<ItemPickupPoint> findByStoreIdAndItemSkuAndPickupPointCodeInAndMarkForDeleteFalse(String storeId, String itemSku,
      List<String> pickupPointCode);

  List<ItemPickupPoint> findByStoreIdAndPickupPointCode(String storeId, String pickupPointCode);

  List<ItemPickupPoint> findByStoreIdAndPickupPointCodeAndMarkForDeleteFalse(String storeId, String pickupPointCode);

  Long countByStoreIdAndPickupPointCodeAndMarkForDeleteFalse(String storeId, String pickupPointCode);

  List<ItemPickupPoint> findByStoreIdAndItemSkuAndMerchantCodeAndMarkForDeleteFalse(String storeId,
    String itemSku, String merchantCode);

  Page<ItemPickupPoint> findByStoreIdAndItemSkuInAndCncActiveTrueAndMarkForDeleteFalse(
    String storeId, Set<String> itemSkusFromMultipleMerchants, Pageable pageable);

  List<ItemPickupPoint> findByStoreIdAndItemSkuInAndCncActiveAndMarkForDeleteFalse(String storeId,
    Set<String> itemSkus, boolean cncActive);

  /**
   * @param storeId must not be blank
   * @param productSku must not be blank
   * @param delivery must not be blank
   * @return ItemPickupPoint
   */
  List<ItemPickupPoint> findItemPickupPointByStoreIdAndProductSkuAndDeliveryAndMarkForDeleteFalse(String storeId,
    String productSku, boolean delivery);

  ItemPickupPoint findFirstByStoreIdAndItemSkuAndCncActiveAndMarkForDeleteFalse(String storeId,
    String itemSku, boolean cncActive);

  ItemPickupPoint findFirstByStoreIdAndItemSkuAndCncActiveAndPickupPointCodeNotInAndMarkForDeleteFalse(String storeId,
      String itemSku, boolean cncActive, Set<String> pickupPointCodes);

  ItemPickupPoint findFirstByStoreIdAndItemSkuAndMarkForDeleteFalse(String storeId, String itemSku);

  ItemPickupPoint findFirstByStoreIdAndProductSkuAndCncActiveAndMarkForDeleteFalse(String storeId,
      String productSku, boolean cncActive);

  ItemPickupPoint findFirstByStoreIdAndProductSkuAndCncActiveAndPickupPointCodeNotInAndMarkForDeleteFalse(String storeId,
      String productSku, boolean cncActive, Set<String> pickupPointCodes);

  ItemPickupPoint findFirstByStoreIdAndProductSkuAndMarkForDeleteFalse(String storeId, String productSku);

  ItemPickupPoint findByStoreIdAndItemSkuAndPickupPointCodeAndCncActiveAndMarkForDeleteFalse(String storeId, String itemSku,
    String pickupPointCode, boolean cncActive);

  List<ItemPickupPoint> findByStoreIdAndOfflineItemIdInAndCncActiveTrueAndMarkForDeleteFalse(String storeId,
    List<String> offlineItemIdList);

  List<ItemPickupPoint> findByStoreIdAndProductSkuAndPickupPointCodeAndMarkForDeleteFalse(String storeId,
      String productSku, String pickupPointCode);

  List<ItemPickupPoint> findByStoreIdAndProductSkuAndPickupPointCode(String storeId,
      String productSku, String pickupPointCode);

  List<ItemPickupPoint> findByStoreIdAndProductSkuInAndMarkForDeleteFalse(String storeId, List<String> productSkuList);

  List<ItemPickupPoint> findByStoreIdAndProductSkuInAndMarkForDeleteFalseAndDeliveryTrue(String storeId, List<String> productSkuList);

  Page<ItemPickupPoint> findByStoreIdAndItemSkuInAndMarkForDeleteFalse(String storeId, List<String> itemSkus, Pageable pageable);

  @Query("{ 'storeId': ?0, 'itemSku': { $in: ?1 }, "
      + "  $or: [ { 'distribution': false }, { 'distribution': { $exists: false } } ], "
      + "  'markForDelete': false }")
  Page<ItemPickupPoint> findByStoreIdAndItemSkuInAndDistributionFalseOrMissingAndMarkForDeleteFalse(
      String storeId, List<String> itemSkus, Pageable pageable);

  List<ItemPickupPoint> findByStoreIdAndItemSkuInAndDistributionTrueAndMarkForDeleteFalse(String storeId,
      List<String> itemSkus);

  Page<ItemPickupPoint> findByStoreIdAndItemSkuInAndMarkForDeleteFalseAndFbbActivated(String storeId,
      List<String> itemSku, Pageable pageable, boolean fbbActivated);

  Page<ItemPickupPoint> findByStoreIdAndProductSkuInAndMarkForDeleteFalse(String storeId, List<String> productSkus, Pageable pageable);

  Page<ItemPickupPoint> findByStoreIdAndProductSkuInAndMarkForDeleteFalseAndFbbActivated(String storeId,
      List<String> productSku, Pageable pageable, boolean fbbActivated);

  Long countByStoreIdAndProductSkuAndMarkForDeleteFalse(String storeId, String productSku);

  Long countByStoreIdAndProductSkuAndMarkForDeleteFalseAndFbbActivated(String storeId, String itemSku, boolean fbbActivated);

  List<ItemPickupPoint> findByStoreIdAndItemSkuInAndPickupPointCodeAndMarkForDeleteFalse(String storeId,
      Set<String> itemSkus, String pickupPointCode);

  Page<ItemPickupPoint> findByStoreIdAndProductSkuInAndMarkForDeleteFalseAndDeliveryTrue(
    String storeId, String productSku, Pageable pageable);

  /**
   * Delete By StoreId And ProductSkus
   *
   * @param storeId
   * @param productSku
   * @return
   */
  List<ItemPickupPoint> deleteByStoreIdAndProductSkuIn(String storeId, Set<String> productSku);

  List<ItemPickupPoint> findByStoreIdAndProductSkuInAndPickupPointCodeInAndMarkForDeleteFalse(String storeId,
      List<String> productSkuList, List<String> pickupPointCodes);

  /**
   * Return all active L5s for a merchant and pp code
   * @param storeId
   * @param merchantCode
   * @param pickupPointCode
   * @return
   */
  Page<ItemPickupPoint> findByStoreIdAndMerchantCodeAndPickupPointCodeAndMarkForDeleteFalse(String storeId,
    String merchantCode, String pickupPointCode, Pageable pageable);

  List<ItemPickupPoint> findByItemSkuInAndFbbActivatedTrueAndMarkForDeleteFalse(Set<String> itemSkus);

  List<ItemPickupPoint> findByProductSkuAndOfflineItemIdNotInAndMarkForDeleteFalse(String productSku, Set<String> offlineItemIds);

  Page<ItemPickupPoint> findByStoreIdAndProductSkuInAndMarkForDeleteFalseAndCncActiveTrue(
      String storeId, List<String> productSkus, Pageable pageable);

  Page<ItemPickupPoint> findByStoreIdAndProductSkuIn(String storeId, List<String> productSkus, Pageable pageable);

  Page<ItemPickupPoint> findByStoreIdAndItemSkuIn(String storeId, List<String> itemSkus, Pageable pageable);

}
