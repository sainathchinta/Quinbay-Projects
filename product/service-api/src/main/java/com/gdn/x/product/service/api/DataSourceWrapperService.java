package com.gdn.x.product.service.api;

import java.util.List;
import java.util.Set;

import com.gdn.x.product.model.entity.Item;
import com.gdn.x.product.model.entity.ItemPickupPoint;
import com.gdn.x.product.model.entity.Product;

public interface DataSourceWrapperService {

  //product related fetch queries
  Product getProductByStoreIdAndProductSku(String storeId, String productSku, boolean primaryReadEnabled);

  Product findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(String storeId, String productSku, boolean primaryReadEnabled);

  //item pickup point fetch queries
  List<ItemPickupPoint> findItemPickupPointByStoreIdAndItemSkuInAndMarkForDeleteFalse(String storeId,
      List<String> itemSkus, boolean primaryReadEnabled);

  ItemPickupPoint findItemPickupPointByItemSkuAndPickupPointCode(String storeId, String itemSku, String pickupPointCode, boolean primaryReadEnabled);

  List<ItemPickupPoint> findItemPickupPointByStoreIdAndItemSkuAndMarkForDeleteFalse(String storeId, String itemSku, boolean primaryReadEnabled);

  List<ItemPickupPoint> findItemPickupPointByStoreIdAndItemSkuInAndMarkForDeleteFalseAndFbbActivated(String storeId,
      Set<String> itemSkus, boolean fbbActivated, boolean primaryReadEnabled);

  ItemPickupPoint findItemPickupPointByStoreIdAndItemSkuAndPickupPointCodeAndMarkForDeleteFalse(String storeId, String itemSku, String pickupPointCode, boolean primaryReadEnabled);

  List<ItemPickupPoint> findItemPickupPointByStoreIdAndProductSku(String storeId, String productSku, boolean primaryReadEnabled);

  List<ItemPickupPoint> findItemPickupPointsByProductSkuAndPickupPointCodes(String storeId,
      String productSku, List<String> pickupPointCodes, boolean primaryReadEnabled);

  List<ItemPickupPoint> getItemPickupPointsByProductSkuAndMarkForDeleteFalse(String storeId, String productSku, boolean primaryReadEnabled);

  Long findItemPickupPointCountByStoreIdAndItemSkuAndMarkForDeleteFalse(String storeId, String itemSku, boolean primaryReadEnabled);

  Long findItemPickupPointCountByStoreIdAndItemSkuAndMarkForDeleteFalseAndCncActive(String storeId, String itemSku,
      boolean cncActive, boolean primaryReadEnabled);


  Long findItemPickupPointCountByStoreIdAndItemSkuAndMarkForDeleteFalseAndCncViewConfig(String storeId, String itemSku);

  //item fetch queries
  Item findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(String storeId, String itemSku,
      boolean primaryReadEnabled);

  Item findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(String storeId, String itemSku, boolean fbbActive, boolean primaryReadEnabled);

  List<Item> findItemsByStoreIdAndProductSkuAndMarkForDeleteFalse(String storeId, String productSku, boolean markForDelete, boolean fbbActive, boolean primaryReadEnabled);

  List<Item> findItemByStoreIdAndItemSkus(String storeId, Set<String> itemSkus, boolean primaryReadEnabled);

  Long findItemCountByStoreIdAndProductSkuAndMarkForDeleteFalseAndCncActivated(String storeId, String productSku,
      boolean cncActivated, boolean primaryReadEnabled);
}
