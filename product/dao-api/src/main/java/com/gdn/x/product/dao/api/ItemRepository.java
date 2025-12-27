package com.gdn.x.product.dao.api;

import java.util.Collection;
import java.util.Date;
import java.util.List;
import java.util.Set;
import java.util.stream.Stream;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Slice;
import org.springframework.data.mongodb.repository.MongoRepository;

import com.gdn.x.product.model.entity.Item;
import com.gdn.x.product.model.entity.PristineDataItem;


public interface ItemRepository extends MongoRepository<Item, String>, ItemRepositoryCustom {

  /**
   * method for get all items
   * @param storeId  must not be blank
   * @return Stream of Item
   */
  Stream<Item> streamAllByStoreId(String storeId);

  /**
   * method for get items based on given itemSkus
   * @param storeId  must not be blank
   * @param itemSkus must not be empty
   * @return Stream of Item
   */
  Stream<Item> streamAllByStoreIdAndItemSkuIn(String storeId, List<String> itemSkus);

  /**
   * @param storeId must not be blank
   * @param itemSkus must not be blank
   * @return Item
   */
  List<Item> findItemsByStoreIdAndItemSkuInAndMarkForDeleteFalse(String storeId,
      Set<String> itemSkus);

  /**
   *
   * @param storeId
   * @param itemSkus
   * @return
   */
  List<Item> findItemsByStoreIdAndItemSkuIn(String storeId, Set<String> itemSkus);

  /**
   * @param storeId must not be blank
   * @param merchantSku must not be blank
   * @return list of items
   */
  List<Item> findItemsByStoreIdAndMerchantSkuAndMarkForDeleteFalse(String storeId,
      String merchantSku);

  /**
   * @param storeId must not be blank
   * @param merchantCode must not be blank
   * @param merchantSkus must not be blank
   * @return list of items
   */
  List<Item> findByStoreIdAndMerchantCodeAndMerchantSkuInAndMarkForDeleteFalseAndIsArchivedFalse(String storeId,
      String merchantCode, List<String> merchantSkus);


  /**
   * @param storeId must not be blank
   * @param productSku must not be blank
   * @return never null
   */
  List<Item> findItemsByStoreIdAndProductSku(String storeId, String productSku);

  /**
   * @param storeId must not be blank
   * @param productSku must not be blank
   * @return never null
   */
  List<Item> findItemsByStoreIdAndProductSkuAndMarkForDeleteFalse(String storeId, String productSku);


  /**
   * @param storeId must not be blank
   * @param productSkus must not be blank
   * @return never null
   */
  List<Item> findItemsByStoreIdAndProductSkuInAndMarkForDeleteFalseAndIsArchivedFalse(String storeId,
      Set<String> productSkus);


  /**
   * Find items without MFD and archive check
   *
   * @param storeId
   * @param productSkus
   * @param pageable
   * @return
   */
  Page<Item> findItemsByStoreIdAndProductSkuIn(String storeId, Set<String> productSkus, Pageable pageable);



  /**
   * Find items without MFD and archive check
   *
   * @param storeId
   * @param productSkus
   * @return
   */
  List<Item> findItemsByStoreIdAndProductSkuIn(String storeId, Set<String> productSkus);

  /**
   * update categoryCode in item by itemSkus
   *
   * @param storeId
   * @param itemSkuList
   * @param categoryCode
   * @return
   */
  List<Item> updateCategoryCodeByItemSkus(String storeId, List<String> itemSkuList, String categoryCode);

  /**
   * Get Product itemSkus based on storeId, list of itemCodes and markForDelete false
   * @param storeId
   * @param itemCodes must not be blank L2 ids
   * @return list of items
   */
  List<Item> findItemsByStoreIdAndItemCodeInAndMarkForDeleteFalse(String storeId,
      Set<String> itemCodes);

  /**
   * Get Product itemSkus based on storeId, list of itemCodes and markForDelete false,
   * isSynchronized true and pageable
   *
   * @param storeId
   * @param ItemCode
   * @param pageable
   * @return
   */
  Page<Item> findByStoreIdAndItemCodeAndMarkForDeleteFalseAndIsSynchronizedTrue(String storeId,
      String ItemCode, Pageable pageable);

  /**
   * Get all items by merchant code
   * @param storeId
   * @param merchantCode
   * @return
   */
  List<Item> findItemsByStoreIdAndMerchantCodeAndMarkForDeleteFalse(String storeId, String merchantCode);

  /**
   * @param storeId
   * @param merchantCode
   * @param pageable
   * @return
   */
  Page<Item> findItemsByStoreIdAndMerchantCodeAndMarkForDeleteFalseAndIsArchivedFalse(String storeId,
      String merchantCode, Pageable pageable);

  /**
   * Get all item by item code
   * @param storeId
   * @param ItemCode
   * @return
   */
  List<Item> findByStoreIdAndItemCodeAndMarkForDeleteFalse(String storeId, String ItemCode);


  /**
   * Get All Items by pristine list
   * @param storeId
   * @param pristineDataItems
   * @return
   */
  List<Item> findByStoreIdAndPristineDataItemInAndMarkForDeleteFalseAndIsArchivedFalse(String storeId,
      List<PristineDataItem> pristineDataItems);

  /**
   * Get All Items by pristine list
   *
   * @param storeId
   * @param pristineDataItems
   * @return
   */
  List<Item> findByStoreIdAndPristineDataItemIn(String storeId, List<PristineDataItem> pristineDataItems);

  /**
   * Get All items by pristine
   * @param storeId
   * @param pristineDataItem
   * @return
   */
  List<Item> findByStoreIdAndPristineDataItemAndMarkForDeleteFalseAndIsArchivedFalse(String storeId,
      PristineDataItem pristineDataItem);

  List<Item> findItemsByStoreIdAndProductSkuAndCncActivatedAndMarkForDeleteFalse(String storeId, String productSku, boolean cncActivated);

  /**
   * Get All items by product sku list where off2On channel is active
   * @param storeId
   * @param productSkus
   * @return list of items
   */
  List<Item> findItemsByStoreIdAndProductSkuInAndMarkForDeleteFalseAndIsArchivedFalseAndOff2OnChannelActiveTrue(String storeId,
      Set<String> productSkus);

  /**
   * Get items by storeId , updatedBy and pageable
   * @param storeId
   * @param updatedBy
   * @param pageable
   * @return
   */
  Slice<Item> findByStoreIdAndUpdatedByAndMarkForDeleteFalse(String storeId, String updatedBy,
      Pageable pageable);

  /**
   * Get updated items
   *
   * @param storeId
   * @param lastIndexTime
   * @param pageable
   * @return
   */
  Page<Item> findByStoreIdAndUpdatedDateGreaterThan(String storeId, Date lastIndexTime, Pageable pageable);


  /**
   * Get Paginated response as list of Items by productSku and mark for delete as false
   *
   * @param storeId
   * @param productSkus
   * @param pageable
   * @return
   */
  Page<Item> findItemsByStoreIdAndProductSkuInAndMarkForDeleteFalse(String storeId,
    Set<String> productSkus, Pageable pageable);

  List<Item> findItemsByStoreIdAndProductSkuInAndMarkForDelete(String storeId,
      Set<String> productSkus, boolean markForDelete);

  /**
   * Delete Items By StoreId And ProductSkus
   *
   * @param storeId
   * @param productSku
   * @return
   */
  List<Item> deleteByStoreIdAndProductSkuIn(String storeId, Set<String> productSku);

  /**
   *
   * Get item by merchant code and merchant sku
   *
   * @param storeId
   * @param merchantCode
   * @param merchantSku
   * @return Item
   */
  Item findFirstItemByStoreIdAndMerchantCodeAndMerchantSkuAndMarkForDeleteFalse(String storeId,
      String merchantCode, String merchantSku);

  /**
   *
   * Get item by merchant code and merchant sku
   *
   * @param storeId
   * @param merchantCode
   * @param merchantSku
   * @return Item
   */
  Item findFirstItemByStoreIdAndMerchantCodeAndProductSkuInAndMerchantSku(String storeId,
      String merchantCode, List<String> productSkus, String merchantSku);

  List<Item> findByStoreIdAndItemSkuInAndMarkForDeleteFalse(String storeId, Set<String> itemSkus);

  List<Item> findByStoreIdAndItemCodeIn(String storeId, Collection<String> itemCodes);

  boolean existsByStoreIdAndUpcCodeAndMerchantCodeAndMarkForDeleteFalseAndIsArchivedFalse(String storeId, String upcCode,
      String merchantCode);
}
