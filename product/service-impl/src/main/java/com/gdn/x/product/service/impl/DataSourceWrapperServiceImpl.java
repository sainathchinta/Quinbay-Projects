package com.gdn.x.product.service.impl;

import java.util.List;
import java.util.Set;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.gdn.x.product.enums.Constants;
import com.gdn.x.product.model.entity.Item;
import com.gdn.x.product.model.entity.ItemPickupPoint;
import com.gdn.x.product.model.entity.Product;
import com.gdn.x.product.service.api.DataSourceWrapperService;
import com.gdn.x.product.service.api.ItemCacheableService;
import com.gdn.x.product.service.api.ItemPickupPointService;
import com.gdn.x.product.service.api.ItemService;
import com.gdn.x.product.service.api.ProductService;

import lombok.extern.slf4j.Slf4j;

@Service
@Slf4j
public class DataSourceWrapperServiceImpl implements DataSourceWrapperService {

  @Autowired
  private ProductService productService;

  @Autowired
  private ItemService itemService;

  @Autowired
  private ItemPickupPointService itemPickupPointService;

  @Autowired
  private ItemCacheableService itemCacheableService;

  @Override
  public Product getProductByStoreIdAndProductSku(String storeId, String productSku, boolean primaryReadEnabled) {
    if (primaryReadEnabled) {
      return productService.getProductReadFromPrimary(storeId, productSku);
    } else {
      return productService.getProduct(storeId, productSku);
    }
  }

  @Override
  public Product findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(String storeId, String productSku,
      boolean primaryReadEnabled) {
    if (primaryReadEnabled) {
      return productService.findProductByStoreIdAndProductSkuAndMarkForDeleteFalseReadFromPrimary(storeId, productSku);
    } else {
      return productService.findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(storeId, productSku);
    }
  }

  @Override
  public List<ItemPickupPoint> findItemPickupPointByStoreIdAndItemSkuInAndMarkForDeleteFalse(String storeId,
      List<String> itemSkus, boolean primaryReadEnabled) {
    if (primaryReadEnabled) {
      return itemPickupPointService.findByStoreIdAndItemSkuInAndMarkForDeleteFalseReadFromPrimary(storeId, itemSkus);
    } else {
      return itemPickupPointService.findByStoreIdAndItemSkuInAndMarkForDeleteFalse(storeId, itemSkus);
    }
  }

  @Override
  public ItemPickupPoint findItemPickupPointByItemSkuAndPickupPointCode(String storeId, String itemSku,
      String pickupPointCode, boolean primaryReadEnabled) {
    if (primaryReadEnabled) {
      return itemPickupPointService.findByItemSkuAndPickupPointCodeReadFromPrimary(storeId, itemSku, pickupPointCode);
    } else {
      return itemPickupPointService.findByItemSkuAndPickupPointCode(storeId, itemSku, pickupPointCode);
    }
  }

  @Override
  public List<ItemPickupPoint> findItemPickupPointByStoreIdAndItemSkuAndMarkForDeleteFalse(String storeId,
      String itemSku, boolean primaryReadEnabled) {
    if (primaryReadEnabled) {
      return itemPickupPointService.findByStoreIdAndItemSkuAndMarkForDeleteFalseReadFromPrimary(storeId, itemSku);
    } else {
      return itemPickupPointService.findByStoreIdAndItemSkuAndMarkForDeleteFalse(storeId, itemSku);
    }
  }

  @Override
  public List<ItemPickupPoint> findItemPickupPointByStoreIdAndItemSkuInAndMarkForDeleteFalseAndFbbActivated(
      String storeId, Set<String> itemSkus, boolean fbbActivated, boolean primaryReadEnabled) {
    if (primaryReadEnabled) {
      return itemPickupPointService.findByStoreIdAndItemSkuInAndMarkForDeleteFalseAndFbbActivatedReadFromPrimary(
          storeId, itemSkus, fbbActivated);
    } else {
      return itemPickupPointService.findByStoreIdAndItemSkuInAndMarkForDeleteFalseAndFbbActivated(storeId, itemSkus,
          fbbActivated);
    }
  }

  @Override
  public ItemPickupPoint findItemPickupPointByStoreIdAndItemSkuAndPickupPointCodeAndMarkForDeleteFalse(String storeId,
      String itemSku, String pickupPointCode, boolean primaryReadEnabled) {
    if (primaryReadEnabled) {
      return itemPickupPointService.findByStoreIdAndItemSkuAndPickupPointCodeAndMarkForDeleteFalseReadFromPrimary(
          storeId, itemSku, pickupPointCode);
    } else {
      return itemPickupPointService.findByStoreIdAndItemSkuAndPickupPointCodeAndMarkForDeleteFalse(storeId, itemSku,
          pickupPointCode);
    }
  }

  @Override
  public List<ItemPickupPoint> findItemPickupPointByStoreIdAndProductSku(String storeId, String productSku,
      boolean primaryReadEnabled) {
    if (primaryReadEnabled) {
      return itemPickupPointService.findByStoreIdAndProductSkuReadFromPrimary(storeId, productSku);
    } else {
      return itemPickupPointService.findByStoreIdAndProductSku(storeId, productSku);
    }
  }

  @Override
  public List<ItemPickupPoint> findItemPickupPointsByProductSkuAndPickupPointCodes(String storeId, String productSku,
      List<String> pickupPointCodes, boolean primaryReadEnabled) {
    if (primaryReadEnabled) {
      return itemPickupPointService.findItemPickupPointsByProductSkuAndPickupPointCodesReadFromPrimary(storeId,
          productSku, pickupPointCodes);
    } else {
      return itemPickupPointService.findItemPickupPointsByProductSkuAndPickupPointCodes(storeId, productSku,
          pickupPointCodes);
    }
  }

  @Override
  public List<ItemPickupPoint> getItemPickupPointsByProductSkuAndMarkForDeleteFalse(String storeId, String productSku,
      boolean primaryReadEnabled) {
    if (primaryReadEnabled) {
      return itemPickupPointService.getItemPickupPointsByProductSkuAndMarkForDeleteFalseReadFromPrimary(storeId,
          productSku);
    } else {
      return itemPickupPointService.getItemPickupPointsByProductSkuAndMarkForDeleteFalse(storeId, productSku);
    }
  }

  @Override
  public Long findItemPickupPointCountByStoreIdAndItemSkuAndMarkForDeleteFalse(String storeId, String itemSku,
      boolean primaryReadEnabled) {
    if (primaryReadEnabled) {
      return itemPickupPointService.findCountByStoreIdAndItemSkuAndMarkForDeleteFalseReadFromPrimary(storeId, itemSku);
    } else {
      return itemPickupPointService.findCountByStoreIdAndItemSkuAndMarkForDeleteFalse(storeId, itemSku);
    }
  }

  @Override
  public Long findItemPickupPointCountByStoreIdAndItemSkuAndMarkForDeleteFalseAndCncActive(
      String storeId, String itemSku, boolean cncActive, boolean primaryReadEnabled) {
    if (primaryReadEnabled) {
      return itemPickupPointService.findCountByStoreIdAndItemSkuAndMarkForDeleteFalseAndCncActiveReadFromPrimary(
          storeId, itemSku, cncActive);
    } else {
      return itemPickupPointService.findCountByStoreIdAndItemSkuAndMarkForDeleteFalseAndCncActive(storeId, itemSku,
          cncActive);
    }
  }

  @Override
  public Long findItemPickupPointCountByStoreIdAndItemSkuAndMarkForDeleteFalseAndCncViewConfig(String storeId,
      String itemSku) {
    return itemPickupPointService.findCountByStoreIdAndItemSkuAndMarkForDeleteFalseAndCncViewConfig(storeId, itemSku,
        Constants.CNC);
  }

  @Override
  public Item findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(String storeId, String itemSku,
      boolean primaryReadEnabled) {
    if (primaryReadEnabled) {
      return itemService.findByStoreIdAndItemSkuAndMarkForDeleteFalseReadFromPrimary(storeId, itemSku);
    } else {
      return itemService.findByStoreIdAndItemSkuAndMarkForDeleteFalse(storeId, itemSku);
    }
  }

  @Override
  public Item findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(String storeId, String itemSku, boolean fbbActive,
      boolean primaryReadEnabled) {
    if (primaryReadEnabled) {
      return itemService.findByStoreIdAndItemSkuAndMarkForDeleteFalseReadFromPrimary(storeId, itemSku);
    } else {
      return itemCacheableService.findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(storeId, itemSku, false, false,
          fbbActive);
    }
  }

  @Override
  public List<Item> findItemsByStoreIdAndProductSkuAndMarkForDeleteFalse(String storeId, String productSku,
      boolean markForDelete, boolean fbbActive, boolean primaryReadEnabled) {
    if (primaryReadEnabled) {
      return itemService.findItemsByStoreIdAndProductSkuAndMarkForDeleteReadFromPrimary(storeId, productSku,
          markForDelete);
    } else {
      return itemCacheableService.findItemsByStoreIdAndProductSkuAndMarkForDeleteFalse(storeId, productSku, false,
          false, fbbActive);
    }
  }

  @Override
  public List<Item> findItemByStoreIdAndItemSkus(String storeId, Set<String> itemSkus, boolean primaryReadEnabled) {
    if (primaryReadEnabled) {
      return itemService.findByStoreIdAndItemSkusReadFromPrimary(storeId, itemSkus);
    } else {
      return itemService.findByStoreIdAndItemSkus(storeId, itemSkus);
    }
  }

  @Override
  public Long findItemCountByStoreIdAndProductSkuAndMarkForDeleteFalseAndCncActivated(String storeId, String productSku,
      boolean cncActivated, boolean primaryReadEnabled) {
    if (primaryReadEnabled) {
      return itemService.findCountByStoreIdAndProductSkuAndMarkForDeleteFalseAndCncActivatedReadFromPrimary(storeId,
          productSku, cncActivated);
    } else {
      return itemService.findCountByStoreIdAndProductSkuAndMarkForDeleteFalseAndCncActivated(storeId, productSku,
          cncActivated);
    }
  }
}
