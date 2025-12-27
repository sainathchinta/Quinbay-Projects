package com.gdn.x.product.service.impl;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.stream.Collectors;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Lazy;
import org.springframework.stereotype.Service;
import com.gdn.x.product.enums.SystemParameterNames;
import com.gdn.x.product.model.entity.Item;
import com.gdn.x.product.model.entity.ItemPickupPoint;
import com.gdn.x.product.model.entity.MasterDataItem;
import com.gdn.x.product.model.entity.MasterDataProduct;
import com.gdn.x.product.model.entity.Product;
import com.gdn.x.product.model.vo.MasterDataDetailWithProductAndItemsResponseVo;
import com.gdn.x.product.model.vo.ProductAndItemsVO;
import com.gdn.x.product.service.annotation.SolrIndex;
import com.gdn.x.product.service.api.CacheEvictHelperService;
import com.gdn.x.product.service.api.CacheEvictItemService;
import com.gdn.x.product.service.api.CacheItemHelperService;
import com.gdn.x.product.service.api.ItemCacheableService;
import com.gdn.x.product.service.api.ItemPickupPointService;
import com.gdn.x.product.service.api.ObjectConverterService;
import com.gdn.x.product.service.api.ProductAndItemSolrIndexerService;
import com.gdn.x.product.service.api.ProductService;
import com.gdn.x.product.service.api.SolrIndexService;
import com.gdn.x.product.service.api.SystemParameterService;

@Service
public class SolrIndexServiceImpl implements SolrIndexService {

  @Autowired
  private CacheEvictHelperService cacheEvictHelperService;

  @Autowired
  @Lazy
  private ProductService productService;

  @Autowired
  @Lazy
  private ItemCacheableService itemService;

  @Autowired
  private SystemParameterService systemParameterService;

  @Autowired
  private ItemPickupPointService itemPickupPointService;

  @Autowired
  private CacheEvictItemService cacheEvictItemService;

  @Autowired
  private ObjectConverterService objectConverterService;

  @Autowired
  private CacheItemHelperService cacheItemHelperService;

  @Autowired
  @Lazy
  private ProductAndItemSolrIndexerService productAndItemSolrIndexerService;

  private List<ProductAndItemsVO> getAllProductData(String storeId, List<String> productSku,
      boolean clearCache) {
    List<Product> products =
        this.productService.findByStoreIdAndProductSkuIn(storeId, productSku);
    List<ProductAndItemsVO> productAndItemsVos = new ArrayList<>();
    List<Item> items = null;
    for (Product product : products) {
      if ((Boolean.valueOf(systemParameterService.findValueByStoreIdAndVariable(storeId,
          SystemParameterNames.FETCH_ALL_ITEMS_BY_PRODUCT_SKU_ENABLED).getValue()))) {
        items = cacheItemHelperService.findCacheableByStoreIdAndProductSku(storeId, product.getProductSku());
      } else {
        items = this.itemService
            .findItemsByStoreIdAndProductSkuAndMarkForDeleteFalse(product.getStoreId(),
                product.getProductSku(), false, false, false);
      }
      overrideL4DetailsFromL5(storeId, items);
      ProductAndItemsVO productAndItemsVo = new ProductAndItemsVO(product, items);
      productAndItemsVos.add(productAndItemsVo);
      if (clearCache) {
        this.cacheEvictHelperService.evictProductData(storeId, product);
        items.stream().forEach(item -> this.cacheEvictHelperService.evictItemData(storeId, item));
        items.stream().forEach(item -> this.cacheEvictItemService.evictFindL5ByItemSku(storeId, item.getItemSku()));
      }
    }
    return productAndItemsVos;
  }

  private void overrideL4DetailsFromL5(String storeId, List<Item> items) {
    List<String> itemSkus = items.stream().map(Item::getItemSku).collect(Collectors.toList());
    List<ItemPickupPoint> itemPickupPoints =
        itemSkus.stream().map(itemSku -> itemPickupPointService.findByItemSkuAndDelivery(storeId, itemSku))
            .filter(Objects::nonNull).collect(Collectors.toList());
    objectConverterService.overrideL4DetailsFromL5(items, itemPickupPoints);
  }

  @Override
  @SolrIndex
  public List<ProductAndItemsVO> indexBulkProduct(String storeId, List<String> productSku,
      boolean clearCache) throws Exception {
    return this.getAllProductData(storeId, productSku, clearCache);
  }

  @Override
  public MasterDataDetailWithProductAndItemsResponseVo indexBulkProductWithMasterData(String storeId,
      List<String> productSkus, Map<String, MasterDataProduct> mapOfMasterDataProduct,
      Map<String, MasterDataItem> mapOfMasterDataItem, boolean clearCache) throws Exception {
    MasterDataDetailWithProductAndItemsResponseVo masterDataDetailWithProductAndItemsResponseVo =
        new MasterDataDetailWithProductAndItemsResponseVo(mapOfMasterDataProduct, mapOfMasterDataItem,
            this.getAllProductData(storeId, productSkus, clearCache));
    productAndItemSolrIndexerService.applyMasterDataDetailWithProductAndItems(
        masterDataDetailWithProductAndItemsResponseVo, false);
    return masterDataDetailWithProductAndItemsResponseVo;
  }

  @Override
  public MasterDataDetailWithProductAndItemsResponseVo getMasterDataDetailWithProductAndItemsResponseVo(
      String storeId, List<String> productSkus,
      Map<String, MasterDataProduct> mapOfMasterDataProduct,
      Map<String, MasterDataItem> mapOfMasterDataItem, boolean clearCache) {
    return new MasterDataDetailWithProductAndItemsResponseVo(mapOfMasterDataProduct, mapOfMasterDataItem,
        this.getAllProductData(storeId, productSkus, clearCache));
  }

  @Override
  public Item updateSolrAndClearCache(String storeId, Item item) {
    cacheEvictHelperService.evictItemData(storeId, item);
    return item;
  }

}
