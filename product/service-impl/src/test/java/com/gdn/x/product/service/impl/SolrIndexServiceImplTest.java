package com.gdn.x.product.service.impl;

import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;
import static org.mockito.MockitoAnnotations.openMocks;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;

import com.gdn.x.product.dao.api.ItemRepository;
import com.gdn.x.product.dao.api.ProductRepository;
import com.gdn.x.product.enums.SystemParameterNames;
import com.gdn.x.product.model.entity.Item;
import com.gdn.x.product.model.entity.ItemPickupPoint;
import com.gdn.x.product.model.entity.Product;
import com.gdn.x.product.model.entity.SystemParameter;
import com.gdn.x.product.model.vo.MasterDataDetailWithProductAndItemsResponseVo;
import com.gdn.x.product.service.api.CacheEvictHelperService;
import com.gdn.x.product.service.api.CacheEvictItemService;
import com.gdn.x.product.service.api.CacheItemHelperService;
import com.gdn.x.product.service.api.ItemCacheableService;
import com.gdn.x.product.service.api.ItemPickupPointService;
import com.gdn.x.product.service.api.ObjectConverterService;
import com.gdn.x.product.service.api.ProductAndItemSolrIndexerService;
import com.gdn.x.product.service.api.ProductService;
import com.gdn.x.product.service.api.SystemParameterService;

public class SolrIndexServiceImplTest {

  private static final String ITEM_SKU = "itemSku";

  private static final String STORE_ID = "storeId";

  private static final String PRODUCT_SKU = "product-sku";

  private ItemPickupPoint itemPickupPoint;

  @InjectMocks
  private SolrIndexServiceImpl solrIndexServiceImpl;

  @Mock
  private ProductRepository productRepository;

  @Mock
  private ItemRepository itemRepository;

  @Mock
  private ItemCacheableService itemCacheableService;

  @Mock
  private ProductService productService;

  @Mock
  private CacheEvictHelperService cacheEvictHelperService;

  @Mock
  private SystemParameterService systemParameterService;

  @Mock
  private ItemPickupPointService itemPickupPointService;

  @Mock
  private ObjectConverterService objectConverterService;

  @Mock
  private CacheEvictItemService cacheEvictItemService;

  @Mock
  private CacheItemHelperService cacheItemHelperService;

  @Mock
  private ProductAndItemSolrIndexerService productAndItemSolrIndexerService;

  @Test
  public void indexBulkProductWithClearCacheTest() throws Exception {
    List<String> productSkus = Arrays.asList(PRODUCT_SKU);
    Product product = new Product(PRODUCT_SKU);
    product.setStoreId(STORE_ID);
    Item item = new Item(ITEM_SKU, PRODUCT_SKU);

    when(this.productService.findByStoreIdAndProductSkuIn(STORE_ID, productSkus))
        .thenReturn(Arrays.asList(product));
    when(this.cacheItemHelperService.findCacheableByStoreIdAndProductSku(STORE_ID, PRODUCT_SKU))
        .thenReturn(Arrays.asList(item));
    when(itemPickupPointService.findByItemSkuAndDelivery(STORE_ID, ITEM_SKU)).thenReturn(itemPickupPoint);
    this.solrIndexServiceImpl.indexBulkProduct(STORE_ID, productSkus, true);
    Mockito.verify(this.systemParameterService).findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.FETCH_ALL_ITEMS_BY_PRODUCT_SKU_ENABLED);
    verify(this.productService).findByStoreIdAndProductSkuIn(STORE_ID, productSkus);
    verify(this.cacheItemHelperService).findCacheableByStoreIdAndProductSku(STORE_ID, PRODUCT_SKU);
    verify(this.cacheEvictHelperService).evictItemData(STORE_ID, item);
    verify(this.cacheEvictHelperService).evictProductData(STORE_ID, product);
    verify(itemPickupPointService).findByItemSkuAndDelivery(STORE_ID, ITEM_SKU);
    verify(objectConverterService)
        .overrideL4DetailsFromL5(Collections.singletonList(item), Collections.singletonList(itemPickupPoint));

  }

  @Test
  public void indexBulkProductWithMasterDataWithClearCacheTest() throws Exception {
    List<String> productSkus = Arrays.asList(PRODUCT_SKU);
    Product product = new Product(PRODUCT_SKU);
    product.setStoreId(STORE_ID);
    Item item = new Item(ITEM_SKU, PRODUCT_SKU);

    when(this.productService.findByStoreIdAndProductSkuIn(STORE_ID, productSkus))
        .thenReturn(Arrays.asList(product));
    when(this.cacheItemHelperService.findCacheableByStoreIdAndProductSku(STORE_ID, PRODUCT_SKU))
        .thenReturn(Arrays.asList(item));
    when(itemPickupPointService.findByItemSkuAndDelivery(STORE_ID, ITEM_SKU)).thenReturn(itemPickupPoint);
    this.solrIndexServiceImpl.indexBulkProductWithMasterData(STORE_ID, productSkus, null, null,
        true);
    Mockito.verify(this.systemParameterService).findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.FETCH_ALL_ITEMS_BY_PRODUCT_SKU_ENABLED);
    verify(this.productService).findByStoreIdAndProductSkuIn(STORE_ID, productSkus);
    verify(this.cacheItemHelperService).findCacheableByStoreIdAndProductSku(STORE_ID, PRODUCT_SKU);
    verify(this.cacheEvictHelperService).evictItemData(STORE_ID, item);
    verify(this.cacheEvictHelperService).evictProductData(STORE_ID, product);
    verify(itemPickupPointService).findByItemSkuAndDelivery(STORE_ID, ITEM_SKU);
    verify(objectConverterService)
        .overrideL4DetailsFromL5(Collections.singletonList(item), Collections.singletonList(itemPickupPoint));
    verify(productAndItemSolrIndexerService).applyMasterDataDetailWithProductAndItems(Mockito.any(
        MasterDataDetailWithProductAndItemsResponseVo.class), Mockito.anyBoolean());
  }

  @Test
  public void indexBulkProductWithMasterDataWithoutClearCacheTest() throws Exception {
    List<String> productSkus = Arrays.asList(PRODUCT_SKU);
    Product product = new Product(PRODUCT_SKU);
    product.setStoreId(STORE_ID);
    Item item = new Item(ITEM_SKU, PRODUCT_SKU);

    when(this.productService.findByStoreIdAndProductSkuIn(STORE_ID, productSkus))
        .thenReturn(Arrays.asList(product));
    when(this.cacheItemHelperService.findCacheableByStoreIdAndProductSku(STORE_ID, PRODUCT_SKU))
        .thenReturn(Arrays.asList(item));
    when(itemPickupPointService.findByItemSkuAndDelivery(STORE_ID, ITEM_SKU)).thenReturn(itemPickupPoint);
    this.solrIndexServiceImpl.indexBulkProductWithMasterData(STORE_ID, productSkus, null, null,
        false);
    Mockito.verify(this.systemParameterService).findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.FETCH_ALL_ITEMS_BY_PRODUCT_SKU_ENABLED);
    verify(this.productService).findByStoreIdAndProductSkuIn(STORE_ID, productSkus);
    verify(this.cacheItemHelperService).findCacheableByStoreIdAndProductSku(STORE_ID, PRODUCT_SKU);
    verify(itemPickupPointService).findByItemSkuAndDelivery(STORE_ID, ITEM_SKU);
    verify(objectConverterService)
        .overrideL4DetailsFromL5(Collections.singletonList(item), Collections.singletonList(itemPickupPoint));
    verify(productAndItemSolrIndexerService).applyMasterDataDetailWithProductAndItems(Mockito.any(
        MasterDataDetailWithProductAndItemsResponseVo.class), Mockito.anyBoolean());
  }

  @Test
  public void indexBulkProductWithoutClearCacheTest() throws Exception {
    List<String> productSkus = Arrays.asList(PRODUCT_SKU);
    Product product = new Product(PRODUCT_SKU);
    product.setStoreId(STORE_ID);
    Item item = new Item(ITEM_SKU, PRODUCT_SKU);

    when(this.productService.findByStoreIdAndProductSkuIn(STORE_ID, productSkus))
        .thenReturn(Arrays.asList(product));
    when(this.cacheItemHelperService.findCacheableByStoreIdAndProductSku(STORE_ID, PRODUCT_SKU))
        .thenReturn(Arrays.asList(item));
    when(itemPickupPointService.findByItemSkuAndDelivery(STORE_ID, ITEM_SKU)).thenReturn(itemPickupPoint);
    this.solrIndexServiceImpl.indexBulkProduct(STORE_ID, productSkus, false);
    Mockito.verify(this.systemParameterService).findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.FETCH_ALL_ITEMS_BY_PRODUCT_SKU_ENABLED);
    verify(this.productService).findByStoreIdAndProductSkuIn(STORE_ID, productSkus);
    verify(this.cacheItemHelperService).findCacheableByStoreIdAndProductSku(STORE_ID, PRODUCT_SKU);
    verify(itemPickupPointService).findByItemSkuAndDelivery(STORE_ID, ITEM_SKU);
    verify(objectConverterService)
        .overrideL4DetailsFromL5(Collections.singletonList(item), Collections.singletonList(itemPickupPoint));
  }

  @Test
  public void indexBulkProductWithoutClearCache_whenFetchAllItemsFalseTest() throws Exception {
    List<String> productSkus = Arrays.asList(PRODUCT_SKU);
    Product product = new Product(PRODUCT_SKU);
    product.setStoreId(STORE_ID);
    Item item = new Item(ITEM_SKU, PRODUCT_SKU);
    Mockito.when(this.systemParameterService
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.FETCH_ALL_ITEMS_BY_PRODUCT_SKU_ENABLED)).thenReturn(
        new SystemParameter(STORE_ID, SystemParameterNames.FETCH_ALL_ITEMS_BY_PRODUCT_SKU_ENABLED,
            "false", ""));
    when(this.productService.findByStoreIdAndProductSkuIn(STORE_ID, productSkus))
        .thenReturn(Arrays.asList(product));
    when(this.itemCacheableService.findItemsByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU, false,
        false, false))
        .thenReturn(Arrays.asList(item));
    when(itemPickupPointService.findByItemSkuAndDelivery(STORE_ID, ITEM_SKU)).thenReturn(itemPickupPoint);
    this.solrIndexServiceImpl.indexBulkProduct(STORE_ID, productSkus, false);
    Mockito.verify(this.systemParameterService).findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.FETCH_ALL_ITEMS_BY_PRODUCT_SKU_ENABLED);
    verify(this.productService).findByStoreIdAndProductSkuIn(STORE_ID, productSkus);
    verify(this.itemCacheableService).findItemsByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU,false,
        false, false);
    verify(itemPickupPointService).findByItemSkuAndDelivery(STORE_ID, ITEM_SKU);
    verify(objectConverterService)
        .overrideL4DetailsFromL5(Collections.singletonList(item), Collections.singletonList(itemPickupPoint));
  }
  @BeforeEach
  public void setUp() throws Exception {
    openMocks(this);
    Mockito.when(this.systemParameterService
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.FETCH_ALL_ITEMS_BY_PRODUCT_SKU_ENABLED)).thenReturn(
        new SystemParameter(STORE_ID, SystemParameterNames.FETCH_ALL_ITEMS_BY_PRODUCT_SKU_ENABLED,
            "true", ""));
    itemPickupPoint = new ItemPickupPoint();
    itemPickupPoint.setItemSku(ITEM_SKU);
    itemPickupPoint.setProductSku(PRODUCT_SKU);
  }

  @AfterEach
  public void tearDown() {
    verifyNoMoreInteractions(this.productRepository);
    verifyNoMoreInteractions(this.itemCacheableService);
    verifyNoMoreInteractions(this.productService);
    verifyNoMoreInteractions(this.itemRepository);
    verifyNoMoreInteractions(this.cacheEvictHelperService, systemParameterService);
    verifyNoMoreInteractions(this.objectConverterService);
  }

  @Test
  public void updateSolrAndClearCacheTest() {
    Item item = new Item(ITEM_SKU, PRODUCT_SKU);
    this.solrIndexServiceImpl.updateSolrAndClearCache(STORE_ID, item);
    verify(this.cacheEvictHelperService).evictItemData(STORE_ID, item);
  }
}
