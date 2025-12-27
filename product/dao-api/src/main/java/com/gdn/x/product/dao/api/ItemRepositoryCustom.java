package com.gdn.x.product.dao.api;

import java.util.Collection;
import java.util.List;
import java.util.Set;

import com.gdn.x.product.model.entity.Item;
import com.gdn.x.product.model.entity.PristineDataItem;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;

public interface ItemRepositoryCustom {

  Item addActivePromoBundling(String storeId, String itemSku, String promoBundlingType);

  /**
   * @param storeId
   * @param itemSkus
   * @param ticketTemplateCode
   * @return
   */
  List<Item> assignTicketTemplate(String storeId, List<String> itemSkus, String ticketTemplateCode);

  List<Item> getItemAvailability(String storeId, Set<String> productSkus);

  /**
   * @param storeId
   * @param itemSkus
   * @return
   */
  List<Item> getPriceAndOff2OnChannelActive(String storeId, List<String> itemSkus);

  /**
   * @param storeId
   * @param itemSkus
   * @return
   */
  List<Item> getPriceAndViewConfigs(String storeId, List<String> itemSkus);

  /**
   * To get item view configs for a set of item skus
   *
   * @param storeId
   * @param itemSkus
   * @return
   */
  List<Item> getItemViewConfigsByItemSkus(String storeId, Set<String> itemSkus);

  Item removeActivePromoBundling(String storeId, String itemSku, String promoBundlingType);

  /**
   * Get items by itemSkus with the required fields
   *
   * @param storeId
   * @param itemSkus
   * @param includes
   * @return
   */
  List<Item> findByStoreIdAndItemSkus(String storeId, Set<String> itemSkus, String... includes);

  List<Item> updateDangerousGoodsByItemSku(String storeId, Set<String> itemSkus, Integer dangerousLevel);

  /**
   * Update specified field, after update succed, the new data will be returned.
   *
   * @param storeId
   * @param itemSku
   * @param fieldName
   * @param value
   * @return
   */
  Item updateFieldByItemSku(String storeId, String itemSku, String fieldName, Object value);

  /**
   * Update specified field, after update succeed, the new data will be returned.
   *
   * @param storeId
   * @param itemSkus
   * @param fieldName
   * @param value
   * @return
   */
  List<Item> updateFieldByItemSkus(String storeId, Collection<String> itemSkus, String fieldName, Object value);

  /**
   * @param storeId
   * @param itemSku
   * @param active
   * @return
   */
  Item updateOff2OnChannelActiveByItemSku(String storeId, String itemSku, boolean active);

  List<Item> updateOff2OnChannelActiveByProductSku(String storeId, String productSku, boolean active);

  List<Item> updateOff2OnChannelActiveByProductSkuAndUpdatedDateAndBy(String storeId, String productSku, boolean active,
      String userName);

  /**
   * get all product item documents in batch where itemCode exist
   *
   * @return
   */
  List<String> getAllItemCodes(String storeId);

  /**
   * get all product item documents which are pristine mapped
   *
   * @return list of item
   */
  List<Item> getItemsWithPristineAvailable();

  /**
   * get all item code which are mapped to particular PristineDataItem
   *
   * @return list of itemCode
   */
  Set<String> getItemCodesByPristine(String storeId, PristineDataItem pristineItem);

  /**
   * @param storeId
   * @param pristineItem
   * @return
   */
  List<Item> findOneItemByPristine(String storeId, PristineDataItem pristineItem);

  /**
   * get pristine Ids by product sku
   *
   * @param storeId
   * @param productSku
   * @return
   */
  List<Item> getByProductSkuAndPristineDataItemExist(String storeId, String productSku);

  Item updateCncActivated(String storeId, String itemSku, boolean cncActivated, String username);

  /**
   * update categoryCode in item by itemSkus
   *
   * @param storeId
   * @param itemSkuList
   * @param categoryCode
   * @return
   */
  List<Item> updateCategoryCodeByItemSkus(String storeId, List<String> itemSkuList, String categoryCode);

  void updateCncActivatedByMerchantCode(String storeId, String merchantCode, boolean cncActivated, String username);

  /**
   * update CncActivated By itemSku
   *
   * @param storeId
   * @param itemSkuSet
   * @param cncActivated
   * @param username
   */
  void updateCncActivatedByItemSkuInMarkForDeleteFalse(String storeId, Set<String> itemSkuSet, boolean cncActivated, String username);

  Set<String> getAllActivePromoBundlingsByItemCode(String storeId, String itemCode);

  Set<String> getAllActivePromoBundlingsByPristine(String storeId, PristineDataItem pristineItem);

  List<Item> findItemSkusByStoreIdAndPristineDataItemInAndCncActivatedTrueAndMarkForDeleteFalseAndIsArchivedFalse(
      String storeId, List<PristineDataItem> pristineDataItems);

  List<Item> findItemSkusByStoreIdAndItemCodeAndCncActivatedTrueAndMarkForDeleteFalseAndIsArchivedFalseAndSynchronizedTrue(
      String storeId, String itemCode);

  List<Item> findSpecificFieldsByStoreIdAndMarkForDeleteFalseAndIsArchivedFalseAndMerchantCode(String storeId, String merchantCode);

  /**
   * Get Items with only specific fields  by productSkus
   *
   * @param storeId
   * @param productSkus
   * @param includes
   * @return
   */
  List<Item> findByStoreIdAndMarkForDeleteFalseAndProductSkus(String storeId, Set<String> productSkus,
      String... includes);

  /**
   * Get items which skus are specified in parameter, with only 2 selected fields, itemCode and itemSku
   *
   * @param storeId  must not be blank
   * @param itemSkus must not be empty
   * @return items with only 2 fields, itemCode and itemSku
   */
  List<Item> findItemCodesByStoreIdAndItemSkuIn(String storeId, Set<String> itemSkus);

  /**
   * Get items which skus are specified in parameter, with only 3 selected fields, itemCode, itemSku and pristineDataItem
   *
   * @param storeId  must not be blank
   * @param itemSkus must not be empty
   * @return items with only 3 fields, itemCode, itemSku and pristineDataItem
   */
  List<Item> findItemCodeAndPristineDataItemByStoreIdAndItemSkuIn(String storeId, Set<String> itemSkus);

  /**
   * Get all itemSkus by item code
   *
   * @param storeId
   * @param itemCode
   * @return
   */
  Set<String> findItemSkusByStoreIdAndItemCodeAndMarkForDeleteFalse(String storeId, String itemCode);

  /**
   * Get all itemSkus by itemCode
   *
   * @param storeId
   * @param itemCode
   * @return
   */
  Set<String> findItemSkusByStoreIdAndItemCode(String storeId, String itemCode);

  /**
   * Get ItemSkus by PristineDatItem
   *
   * @param storeId
   * @param pristineItem
   * @return
   */
  Set<String> findItemSkusByPristine(String storeId, PristineDataItem pristineItem);

  /**
   * Update Item Discount price
   *
   * @param storeId
   * @param item
   */
  void updateItemDiscountPrice(String storeId, Item item);

  /**
   * Update Merchant's Promo discount price
   *
   * @param storeId
   * @param item
   * @return
   */
  Item updateItemMerchantDiscountPrice(String storeId, Item item);

  /**
   * @param storeId
   * @param item
   */
  void updatePristineDataItem(String storeId, Item item);

  /**
   * Update Merchant Promo Discount Active Flag : This item is mapped to promo discount or not
   *
   * @param storeId
   * @param itemSku
   * @param isPromoActive
   * @return
   */
  boolean updateMerchantPromoDiscountFlag(String storeId, String itemSku, boolean isPromoActive);

  /**
   * @param storeId
   * @param itemSkus
   * @return
   */
  List<Item> getItemPriceAndViewConfigsAndPromoDetails(String storeId, List<String> itemSkus);

  /**
   * Get Item details by itemCodes
   *
   * @param itemCodes
   * @return
   */
  List<Item> getItemDetailsByItemCodes(Set<String> itemCodes);

  /**
   * get itemSku by productsku and itemName keyword
   * @param storeId
   * @param keyword
   * @return
   */
  List<Item> getItemSkuByItemNameKeyword(String storeId, String productSku, String keyword);

  /**
   * get itemSku by merchantCode and itemCode and itemName keyword
   * @param storeId
   * @param merchantCode
   * @param itemCodes
   * @param keyword
   * @return
   */
  List<Item> getItemSkuByMerchantCodeAndItemCodeAndItemNameKeyword(String storeId, String merchantCode, Set<String> itemCodes, String keyword);

  /**
   * @param storeId must not be blank
   * @param itemSku must not be blank
   * @param readFromPrimary
   * @return Item
   */
  Item findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(String storeId, String itemSku,
      boolean readFromPrimary);

  /**
   * @param storeId must not be blank
   * @param itemSku must not be blank
   * @return Item
   */
  Item findItemByStoreIdAndItemSku(String storeId, String itemSku, boolean readFromPrimary);

  /**
   *
   * @param storeId
   * @param itemSkus
   * @param readFromPrimary
   * @return
   */
  List<Item> findItemByStoreIdAndItemSkuIn(String storeId, Collection<String> itemSkus, boolean readFromPrimary);

  /**
   * Get all item by item code
   * @param storeId
   * @param itemCodes
   * @return
   */
  List<Item> findByStoreIdAndItemCodeInAndMarkForDeleteFalse(String storeId, Collection<String> itemCodes, boolean readFromPrimary);

  /**
   *
   * @param storeId
   * @param productSku
   * @param markForDelete
   * @return
   */
  List<Item> findItemsByStoreIdAndProductSkuAndMarkForDelete(String storeId, String productSku, boolean markForDelete, boolean readFromPrimary);

  /**
   * Find number of cnc true items
   *
   * @param storeId
   * @param itemSku
   * @param cncActive
   * @return
   */
  Long countByStoreIdAndProductSkuAndMarkForDeleteFalseAndCncActivated(String storeId, String itemSku,
      boolean cncActive, boolean readFromPrimary);

  /**
   * Find list of Items by ItemCode
   *
   * @param storeId   not null
   * @param itemCode  not null
   * @param searchKey productSku or variantName
   * @param page      int default 0
   * @param size      int default 20
   * @param sortBy    sort
   * @param orderBy   direction
   * @return
   */
  Page<Item> getItemSummaryResponsesByItemCodes(String storeId, String itemCode, String searchKey,
    int page, int size, String sortBy, String orderBy);


  /**
   *
   * @param storeId
   * @param productSku
   * @param cncActivated
   * @return
   */
  boolean existsByStoreIdAndProductSkuAndCncActivated(String storeId, String productSku, boolean cncActivated);

  /**
   * @param storeId
   * @param upcCode
   * @param merchantCodes
   * @return
   */

  List<Item> fetchItemsByStoreIdAndUpcCodeAndMerchantCodesAndMarkForDeleteAndIsArchived(String storeId, String upcCode,
      Set<String> merchantCodes, boolean markForDelete, boolean archived);
}

