package com.gdn.x.product.dao.api;

import java.util.List;
import java.util.Set;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.mongodb.repository.MongoRepository;

import com.gdn.x.product.model.entity.OfflineItem;

public interface OfflineItemRepository extends MongoRepository<OfflineItem, String>,
    OfflineItemRepositoryCustom {

  long deleteByStoreIdAndOfflineItemId(String storeId, String offlineItemId);

  List<OfflineItem> findByStoreIdAndMerchantCodeAndPickupPointCode(
      String storeId, String merchantCode, String pickupPointCode);

  List<OfflineItem> findByStoreIdAndMerchantCodeAndItemSku(
      String storeId, String merchantCode, String itemSku);

  Page<OfflineItem> findByStoreIdAndMerchantCodeIn(String storeId, List<String> merchantCode,
      Pageable pageable);

  OfflineItem findByStoreIdAndItemSkuAndPickupPointCodeAndMarkForDeleteFalse(
      String storeId, String itemSku, String pickupPointCode);

  OfflineItem findByStoreIdAndItemSkuAndPickupPointCode(
      String storeId, String itemSku, String pickupPointCode);

  OfflineItem findOneByStoreIdAndItemSkuAndMarkForDeleteFalse(String storeId, String itemSku);

  long deleteByStoreIdAndMerchantCode(String storeId, String merchantCode);

  List<OfflineItem> findByStoreIdAndPickupPointCode(String storeId, String pickupPointCode);

  long deleteByStoreIdAndPickupPointCode(String storeId, String pickupPointCode);

  Page<OfflineItem> findByStoreIdAndItemSkuInAndMarkForDeleteFalse(String storeId, Set<String> itemSku, Pageable pageable);

  Page<OfflineItem> findByStoreIdAndItemSkuAndMarkForDeleteFalse(String storeId, String itemSku, Pageable pageable);

  List<OfflineItem> findByStoreIdAndItemSkuInAndMarkForDeleteFalse(String storeId, Set<String> itemSku);

  List<OfflineItem> findByStoreIdAndItemSkuInAndPickupPointCodeAndMarkForDeleteFalse(String storeId, Set<String> itemSkus, String pickupPointCode);

  List<OfflineItem> findByStoreIdAndMerchantCodeAndMerchantSku(String storeId, String merchantCode, String merchantSku);

  List<OfflineItem> findByStoreIdAndItemSku(String storeId, String itemSku);

  List<OfflineItem> findByStoreIdAndItemSkuIn(String storeId, List<String> itemSku);

  Page<OfflineItem> findByStoreId(String storeId, Pageable pageable);

  List<OfflineItem> findByStoreIdAndItemSkuAndMarkForDeleteFalse(String storeId, String itemSku);

  OfflineItem findByStoreIdAndOfflineItemIdAndMarkForDeleteFalse(String storeId, String offlineItemId);

  List<OfflineItem> findByStoreIdAndOfflineItemIdIn(String storeId, List<String> offlineItemIdList);
}
