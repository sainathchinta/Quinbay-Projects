package com.gdn.x.product.service.api;

import com.gdn.common.exception.ApplicationException;
import com.gdn.x.product.model.entity.Item;
import com.gdn.x.product.model.entity.PristineDataItem;
import com.gdn.x.product.model.vo.CacheItemVO;
import com.gdn.x.product.model.vo.PristineItemAndSiblingsVO;

public interface PristineCacheableService {

  /**
   * @param storeId
   * @param pristineId
   * @return
   */
  PristineItemAndSiblingsVO findPristineItemAndItsSiblingsByPristineId(
      String storeId, String pristineId);

  /**
   *
   * @param storeId
   * @param pristineDataItem
   * @return
   */
  CacheItemVO findItemByPristine(String storeId, PristineDataItem pristineDataItem);

  /**
   *
   * @param pristineDataItem
   * @return
   */
  Item findFirstItemByPristine(PristineDataItem pristineDataItem) throws ApplicationException;

  /**
   * @param storeId
   * @param pristineDataItem
   */
  void evictPristineItemAndSiblingsCacheAndRebuild(String storeId, PristineDataItem pristineDataItem);

  /**
   * @param storeId
   * @param pristineId
   * @return
   */
  boolean evictPristineItemCache(String storeId, String pristineId);
}
