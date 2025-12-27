package com.gdn.x.product.service.cache;

import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;
import static org.mockito.MockitoAnnotations.openMocks;
import static org.mockito.internal.verification.VerificationModeFactory.times;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.springframework.test.util.ReflectionTestUtils;

import com.gdn.x.product.model.entity.Item;
import com.gdn.x.product.model.entity.ItemPickupPoint;
import com.gdn.x.product.model.entity.PristineDataItem;
import com.gdn.x.product.model.entity.Product;
import com.gdn.x.product.service.api.CacheEvictItemService;
import com.gdn.x.product.service.api.CacheEvictProductService;
import com.gdn.x.product.service.api.ItemService;
import com.gdn.x.product.service.api.PristineCacheableService;

/**
 * This unit test class template is the work of maria.o.a.sunaryo and felix.w.wijaya as
 * participation in Blibli Hackathon #1. Generated on 24 Jun 2016 11:28:30
 */
public class CacheEvictHelperServiceImplTest {

  private static final String MERCHANT_CODE = "merchant-code";
  private static final String PRODUCT_CODE = "product-code";
  private static final String STORE_ID = "store-id";
  private static final String MERCHANT_SKU = "merchant-sku";
  private static final String PRODUCT_SKU = "product-sku";
  private static final String ITEM_SKU = "item-sku";
  private static final String ITEM_SKU1 = "item-sku1";
  private static final String PRISTINE_ID = "pristine-id";
  private static final String ITEM_CODE = "item-code";
  private static final String ITEM_CODE1 = "item-code1";
  private static final String PICKUP_POINT_CODE = "pickup-point-code";
  private static final String OFFLINE_ITEM_ID = "offline-item-id";

  @InjectMocks
  private CacheEvictHelperServiceImpl cacheEvictHelperServiceImpl;
  @Mock
  private CacheEvictItemService cacheEvictItemService;
  @Mock
  private CacheEvictProductService cacheEvictProductService;
  @Mock
  private PristineCacheableService pristineCacheableService;

  @Mock
  private ItemService itemService;

  private PristineDataItem pristineDataItem;

  private List<Item> items;

  private Item item;
  private ItemPickupPoint itemPickupPoint;

  @Test
  public void evictItemDataTest() {

    this.cacheEvictHelperServiceImpl.evictItemData(CacheEvictHelperServiceImplTest.STORE_ID, item);
    verify(this.cacheEvictItemService).evictFindItemByItemSku(
        CacheEvictHelperServiceImplTest.STORE_ID, CacheEvictHelperServiceImplTest.ITEM_SKU);
    verify(this.cacheEvictItemService).evictUniqueIdTypeCheck(
        CacheEvictHelperServiceImplTest.STORE_ID, CacheEvictHelperServiceImplTest.ITEM_SKU);
    verify(this.cacheEvictItemService).evictFindItemByProductSku(
        CacheEvictHelperServiceImplTest.STORE_ID, CacheEvictHelperServiceImplTest.PRODUCT_SKU);
    verify(this.cacheEvictItemService).evictFindItemByMerchantSku(
        CacheEvictHelperServiceImplTest.STORE_ID, CacheEvictHelperServiceImplTest.MERCHANT_SKU);
    verify(this.cacheEvictItemService).evictFindItemSkusByPristineId(STORE_ID, PRISTINE_ID);
    verify(this.cacheEvictItemService).evictFindAllActivePromoBundlingsByItemCode(STORE_ID, ITEM_CODE);
    verify(this.cacheEvictItemService)
        .evictFindAllActivePromoBundlingsByPristineId(STORE_ID, PRISTINE_ID);
    verify(pristineCacheableService)
        .evictPristineItemAndSiblingsCacheAndRebuild(STORE_ID, item.getPristineDataItem());
    verify(this.cacheEvictItemService).evictFindAllByStoreIdAndItemCode(STORE_ID, ITEM_CODE);
    verify(cacheEvictItemService)
        .evictFindItemCodesByPristine(STORE_ID, this.pristineDataItem.getPristineId());
  }

  @Test
  public void evictItemDataWithNullMerchantSkuTest() {
    Item item = new Item();
    item.setItemSku(CacheEvictHelperServiceImplTest.ITEM_SKU);
    item.setProductSku(CacheEvictHelperServiceImplTest.PRODUCT_SKU);
    item.setItemCode(CacheEvictHelperServiceImplTest.ITEM_CODE);
    item.setPristineDataItem(this.pristineDataItem);
    this.cacheEvictHelperServiceImpl.evictItemData(CacheEvictHelperServiceImplTest.STORE_ID, item);
    verify(this.cacheEvictItemService).evictFindItemByItemSku(
        CacheEvictHelperServiceImplTest.STORE_ID, CacheEvictHelperServiceImplTest.ITEM_SKU);
    verify(this.cacheEvictItemService).evictUniqueIdTypeCheck(
        CacheEvictHelperServiceImplTest.STORE_ID, CacheEvictHelperServiceImplTest.ITEM_SKU);
    verify(this.cacheEvictItemService).evictFindItemByProductSku(
        CacheEvictHelperServiceImplTest.STORE_ID, CacheEvictHelperServiceImplTest.PRODUCT_SKU);
    verify(this.cacheEvictItemService).evictFindAllActivePromoBundlingsByItemCode(
        CacheEvictHelperServiceImplTest.STORE_ID, CacheEvictHelperServiceImplTest.ITEM_CODE);
    verify(this.cacheEvictItemService)
        .evictFindAllActivePromoBundlingsByPristineId(STORE_ID, PRISTINE_ID);
    verify(this.cacheEvictItemService)
        .evictFindItemSkusByPristineId(STORE_ID, this.pristineDataItem.getPristineId());
    verify(pristineCacheableService)
        .evictPristineItemAndSiblingsCacheAndRebuild(STORE_ID, item.getPristineDataItem());
    verify(this.cacheEvictItemService).evictFindAllByStoreIdAndItemCode(STORE_ID, ITEM_CODE);
    verify(cacheEvictItemService)
        .evictFindItemCodesByPristine(STORE_ID, this.pristineDataItem.getPristineId());
  }

  @Test
  public void evictItemDataWithNullItemCodeTest() {
    Item item = new Item();
    item.setItemSku(CacheEvictHelperServiceImplTest.ITEM_SKU);
    item.setProductSku(CacheEvictHelperServiceImplTest.PRODUCT_SKU);
    item.setItemCode(null);
    item.setPristineDataItem(this.pristineDataItem);
    this.cacheEvictHelperServiceImpl.evictItemData(CacheEvictHelperServiceImplTest.STORE_ID, item);
    verify(this.cacheEvictItemService).evictFindItemByItemSku(
        CacheEvictHelperServiceImplTest.STORE_ID, CacheEvictHelperServiceImplTest.ITEM_SKU);
    verify(this.cacheEvictItemService).evictUniqueIdTypeCheck(
        CacheEvictHelperServiceImplTest.STORE_ID, CacheEvictHelperServiceImplTest.ITEM_SKU);
    verify(this.cacheEvictItemService).evictFindItemByProductSku(
        CacheEvictHelperServiceImplTest.STORE_ID, CacheEvictHelperServiceImplTest.PRODUCT_SKU);
    verify(this.cacheEvictItemService).evictFindAllActivePromoBundlingsByItemCode(
        CacheEvictHelperServiceImplTest.STORE_ID, null);
    verify(this.cacheEvictItemService)
        .evictFindAllActivePromoBundlingsByPristineId(STORE_ID, PRISTINE_ID);
    verify(this.cacheEvictItemService)
        .evictFindItemSkusByPristineId(STORE_ID, this.pristineDataItem.getPristineId());
    verify(pristineCacheableService)
        .evictPristineItemAndSiblingsCacheAndRebuild(STORE_ID, item.getPristineDataItem());
    verify(cacheEvictItemService)
        .evictFindItemCodesByPristine(STORE_ID, this.pristineDataItem.getPristineId());
  }

  @Test
  public void evictProductDataTest() {
    Product product = new Product(CacheEvictHelperServiceImplTest.PRODUCT_SKU);
    product.setMerchantCode(CacheEvictHelperServiceImplTest.MERCHANT_CODE);
    product.setProductCode(CacheEvictHelperServiceImplTest.PRODUCT_CODE);
    this.cacheEvictHelperServiceImpl.evictProductData(CacheEvictHelperServiceImplTest.STORE_ID,
        product);
    verify(this.cacheEvictProductService).evictFindProductByProductCode(
        CacheEvictHelperServiceImplTest.STORE_ID, CacheEvictHelperServiceImplTest.PRODUCT_CODE);
    verify(this.cacheEvictProductService).evictFindProductByProductSku(
        CacheEvictHelperServiceImplTest.STORE_ID, CacheEvictHelperServiceImplTest.PRODUCT_SKU);
  }

  @Test
  public void evictItemCacheTest(){
    this.cacheEvictHelperServiceImpl.evictItemCache(STORE_ID, item);
    Mockito.verify(this.cacheEvictItemService).evictFindItemByItemSku(STORE_ID, item.getItemSku());
    verify(this.cacheEvictItemService).evictUniqueIdTypeCheck(
        CacheEvictHelperServiceImplTest.STORE_ID, item.getItemSku());
    Mockito.verify(this.cacheEvictItemService)
        .evictFindItemByProductSku(STORE_ID, item.getProductSku());
  }

  @Test
  public void evictItemCache_whenItemNullTest(){
    this.cacheEvictHelperServiceImpl.evictItemCache(STORE_ID, null);
  }

  @Test public void evictPristineItemCacheTest() {

    this.cacheEvictHelperServiceImpl
        .evictPristineItemCache(CacheEvictHelperServiceImplTest.STORE_ID, pristineDataItem, items);
    Mockito.verify(pristineCacheableService)
        .evictPristineItemAndSiblingsCacheAndRebuild(CacheEvictHelperServiceImplTest.STORE_ID,
            pristineDataItem);
    Mockito.verify(cacheEvictItemService)
        .evictFindItemSkusByPristineId(CacheEvictHelperServiceImplTest.STORE_ID,
            pristineDataItem.getPristineId());
    Mockito.verify(this.cacheEvictItemService)
        .evictFindItemByItemSku(CacheEvictHelperServiceImplTest.STORE_ID, CacheEvictHelperServiceImplTest.ITEM_SKU1);
    verify(this.cacheEvictItemService).evictUniqueIdTypeCheck(
        CacheEvictHelperServiceImplTest.STORE_ID, CacheEvictHelperServiceImplTest.ITEM_SKU1);
    Mockito.verify(this.cacheEvictItemService)
        .evictFindItemByItemSku(CacheEvictHelperServiceImplTest.STORE_ID, CacheEvictHelperServiceImplTest.ITEM_SKU);
    verify(this.cacheEvictItemService).evictUniqueIdTypeCheck(
        CacheEvictHelperServiceImplTest.STORE_ID, CacheEvictHelperServiceImplTest.ITEM_SKU);
    Mockito.verify(this.cacheEvictItemService, times(2))
        .evictFindItemByProductSku(CacheEvictHelperServiceImplTest.STORE_ID, PRODUCT_SKU);
  }

  @BeforeEach
  public void setUp() throws Exception {
    openMocks(this);

    this.pristineDataItem = new PristineDataItem();
    this.pristineDataItem.setPristineId(PRISTINE_ID);
    item = new Item();
    item.setItemSku(CacheEvictHelperServiceImplTest.ITEM_SKU);
    item.setProductSku(CacheEvictHelperServiceImplTest.PRODUCT_SKU);
    item.setMerchantSku(CacheEvictHelperServiceImplTest.MERCHANT_SKU);
    item.setPristineDataItem(pristineDataItem);
    item.setItemCode(CacheEvictHelperServiceImplTest.ITEM_CODE);

    items = new ArrayList<>();
    Item item1 = new Item();
    item1.setItemSku(CacheEvictHelperServiceImplTest.ITEM_SKU1);
    item1.setProductSku(CacheEvictHelperServiceImplTest.PRODUCT_SKU);
    items.add(item1);
    items.add(item);

    itemPickupPoint = new ItemPickupPoint();
    itemPickupPoint.setItemSku(CacheEvictHelperServiceImplTest.ITEM_SKU);
    itemPickupPoint.setProductSku(CacheEvictHelperServiceImplTest.PRODUCT_SKU);
    itemPickupPoint.setPickupPointCode(CacheEvictHelperServiceImplTest.PICKUP_POINT_CODE);
    itemPickupPoint.setMerchantSku(CacheEvictHelperServiceImplTest.MERCHANT_SKU);

  }

  @Test
  public void evictItemPickupPointDataItemNullTest() {
    this.itemPickupPoint = new ItemPickupPoint();
    itemPickupPoint.setItemSku(ITEM_SKU);
    itemPickupPoint.setPickupPointCode(PICKUP_POINT_CODE);
    itemPickupPoint.setProductSku(PRODUCT_SKU);
    item.setPristineDataItem(null);
    when(itemService.findByStoreIdAndItemSku(STORE_ID, ITEM_SKU)).thenReturn(null);
    this.cacheEvictHelperServiceImpl.evictItemPickupPointData(CacheEvictHelperServiceImplTest.STORE_ID,
        itemPickupPoint,PICKUP_POINT_CODE);
    verify(this.cacheEvictItemService).evictFindItemPickupPointByItemSkuAndPickupPointCode(
        CacheEvictHelperServiceImplTest.STORE_ID, CacheEvictHelperServiceImplTest.ITEM_SKU,PICKUP_POINT_CODE);
    verify(this.cacheEvictItemService).evictUniqueIdTypeCheck(
        CacheEvictHelperServiceImplTest.STORE_ID, CacheEvictHelperServiceImplTest.ITEM_SKU);
    verify(this.cacheEvictItemService).evictFindItemByProductSku(
        CacheEvictHelperServiceImplTest.STORE_ID, CacheEvictHelperServiceImplTest.PRODUCT_SKU);
    verify(this.cacheEvictItemService).evictFindL5ByItemSku(
        CacheEvictHelperServiceImplTest.STORE_ID, CacheEvictHelperServiceImplTest.ITEM_SKU);
  }

  @Test
  public void evictItemPickupPointDataTest() {
    this.itemPickupPoint = new ItemPickupPoint();
    itemPickupPoint.setItemSku(ITEM_SKU);
    itemPickupPoint.setPickupPointCode(PICKUP_POINT_CODE);
    itemPickupPoint.setProductSku(PRODUCT_SKU);
    when(itemService.findByStoreIdAndItemSku(STORE_ID, ITEM_SKU)).thenReturn(item);
    this.cacheEvictHelperServiceImpl.evictItemPickupPointData(CacheEvictHelperServiceImplTest.STORE_ID,
      itemPickupPoint,PICKUP_POINT_CODE);
    verify(this.cacheEvictItemService).evictFindItemPickupPointByItemSkuAndPickupPointCode(
      CacheEvictHelperServiceImplTest.STORE_ID, CacheEvictHelperServiceImplTest.ITEM_SKU,PICKUP_POINT_CODE);
    verify(this.cacheEvictItemService).evictUniqueIdTypeCheck(
      CacheEvictHelperServiceImplTest.STORE_ID, CacheEvictHelperServiceImplTest.ITEM_SKU);
    verify(this.cacheEvictItemService).evictFindItemByProductSku(
      CacheEvictHelperServiceImplTest.STORE_ID, CacheEvictHelperServiceImplTest.PRODUCT_SKU);
    verify(this.cacheEvictItemService).evictFindL5ByItemSku(
        CacheEvictHelperServiceImplTest.STORE_ID, CacheEvictHelperServiceImplTest.ITEM_SKU);
    verify(this.cacheEvictItemService).evictFindAllActivePromoBundlingsByItemCode(STORE_ID, ITEM_CODE, PICKUP_POINT_CODE);
    verify(this.cacheEvictItemService).evictFindAllActivePromoBundlingsByPristineId(STORE_ID, PRISTINE_ID, PICKUP_POINT_CODE);
  }

  @Test
  public void evictItemPickupPointDataPristineDetailsNullTest() {
    this.itemPickupPoint = new ItemPickupPoint();
    itemPickupPoint.setItemSku(ITEM_SKU);
    itemPickupPoint.setPickupPointCode(PICKUP_POINT_CODE);
    itemPickupPoint.setProductSku(PRODUCT_SKU);
    item.setPristineDataItem(null);
    when(itemService.findByStoreIdAndItemSku(STORE_ID, ITEM_SKU)).thenReturn(item);
    this.cacheEvictHelperServiceImpl.evictItemPickupPointData(CacheEvictHelperServiceImplTest.STORE_ID,
        itemPickupPoint,PICKUP_POINT_CODE);
    verify(this.cacheEvictItemService).evictFindItemPickupPointByItemSkuAndPickupPointCode(
        CacheEvictHelperServiceImplTest.STORE_ID, CacheEvictHelperServiceImplTest.ITEM_SKU,PICKUP_POINT_CODE);
    verify(this.cacheEvictItemService).evictUniqueIdTypeCheck(
        CacheEvictHelperServiceImplTest.STORE_ID, CacheEvictHelperServiceImplTest.ITEM_SKU);
    verify(this.cacheEvictItemService).evictFindItemByProductSku(
        CacheEvictHelperServiceImplTest.STORE_ID, CacheEvictHelperServiceImplTest.PRODUCT_SKU);
    verify(this.cacheEvictItemService).evictFindL5ByItemSku(
        CacheEvictHelperServiceImplTest.STORE_ID, CacheEvictHelperServiceImplTest.ITEM_SKU);
    verify(this.cacheEvictItemService).evictFindAllActivePromoBundlingsByItemCode(STORE_ID, ITEM_CODE, PICKUP_POINT_CODE);
  }

  @Test
  public void evictItemPickupPointCacheItemNullTest() {
    this.itemPickupPoint = new ItemPickupPoint();
    itemPickupPoint.setItemSku(ITEM_SKU);
    itemPickupPoint.setProductSku(PRODUCT_SKU);
    itemPickupPoint.setPickupPointCode(PICKUP_POINT_CODE);
    itemPickupPoint.setOfflineItemId(OFFLINE_ITEM_ID);
    Mockito.when(itemService.findByStoreIdAndItemSkus(Mockito.anyString(), Mockito.anySet()))
        .thenReturn(new ArrayList(items));
    this.cacheEvictHelperServiceImpl.evictItemPickupPointCache(CacheEvictHelperServiceImplTest.STORE_ID, null,
        Collections.singletonList(itemPickupPoint));
    verify(this.cacheEvictItemService).evictFindItemPickupPointByItemSkuAndPickupPointCode(
        CacheEvictHelperServiceImplTest.STORE_ID, CacheEvictHelperServiceImplTest.ITEM_SKU,
        itemPickupPoint.getPickupPointCode());
    verify(this.cacheEvictItemService).evictUniqueIdTypeCheck(CacheEvictHelperServiceImplTest.STORE_ID,
        OFFLINE_ITEM_ID);
    verify(this.cacheEvictItemService).evictFindItemByProductSku(CacheEvictHelperServiceImplTest.STORE_ID,
        CacheEvictHelperServiceImplTest.PRODUCT_SKU);
    verify(this.cacheEvictItemService, times(2)).evictFindL5ByItemSku(eq(CacheEvictHelperServiceImplTest.STORE_ID),
        Mockito.anyString());
    verify(this.cacheEvictItemService).evictFindAllActivePromoBundlingsByItemCode(STORE_ID, ITEM_CODE,
        PICKUP_POINT_CODE);
    verify(this.cacheEvictItemService).evictFindAllActivePromoBundlingsByPristineId(STORE_ID, PRISTINE_ID,
        PICKUP_POINT_CODE);
  }

  @Test
  public void evictItemPickupPointCacheTest() {
    this.itemPickupPoint = new ItemPickupPoint();
    itemPickupPoint.setItemSku(ITEM_SKU);
    itemPickupPoint.setProductSku(PRODUCT_SKU);
    itemPickupPoint.setPickupPointCode(PICKUP_POINT_CODE);
    itemPickupPoint.setOfflineItemId(OFFLINE_ITEM_ID);
    when(itemService.findByStoreIdAndItemSkus(STORE_ID, new HashSet<>(Arrays.asList(ITEM_SKU)))).thenReturn(Arrays.asList(item));
    this.cacheEvictHelperServiceImpl.evictItemPickupPointCache(CacheEvictHelperServiceImplTest.STORE_ID, null,
        Collections.singletonList(itemPickupPoint));
    verify(this.cacheEvictItemService).evictFindItemPickupPointByItemSkuAndPickupPointCode(
        CacheEvictHelperServiceImplTest.STORE_ID, CacheEvictHelperServiceImplTest.ITEM_SKU,
        itemPickupPoint.getPickupPointCode());
    verify(this.cacheEvictItemService).evictUniqueIdTypeCheck(CacheEvictHelperServiceImplTest.STORE_ID,
        OFFLINE_ITEM_ID);
    verify(this.cacheEvictItemService).evictFindItemByProductSku(CacheEvictHelperServiceImplTest.STORE_ID,
        CacheEvictHelperServiceImplTest.PRODUCT_SKU);
    verify(this.cacheEvictItemService).evictFindL5ByItemSku(CacheEvictHelperServiceImplTest.STORE_ID,
        CacheEvictHelperServiceImplTest.ITEM_SKU);
    verify(this.cacheEvictItemService).evictFindAllActivePromoBundlingsByItemCode(STORE_ID, ITEM_CODE, PICKUP_POINT_CODE);
    verify(this.cacheEvictItemService).evictFindAllActivePromoBundlingsByPristineId(STORE_ID, PRISTINE_ID, PICKUP_POINT_CODE);
  }

  @Test
  public void evictItemPickupPointCacheItemNonNullTest() {
    this.itemPickupPoint = new ItemPickupPoint();
    itemPickupPoint.setItemSku(ITEM_SKU);
    itemPickupPoint.setProductSku(PRODUCT_SKU);
    itemPickupPoint.setPickupPointCode(PICKUP_POINT_CODE);
    itemPickupPoint.setOfflineItemId(OFFLINE_ITEM_ID);
    this.cacheEvictHelperServiceImpl.evictItemPickupPointCache(CacheEvictHelperServiceImplTest.STORE_ID, Arrays.asList(item),
        Collections.singletonList(itemPickupPoint));
    verify(this.cacheEvictItemService).evictFindItemPickupPointByItemSkuAndPickupPointCode(
        CacheEvictHelperServiceImplTest.STORE_ID, CacheEvictHelperServiceImplTest.ITEM_SKU,
        itemPickupPoint.getPickupPointCode());
    verify(this.cacheEvictItemService).evictUniqueIdTypeCheck(CacheEvictHelperServiceImplTest.STORE_ID,
        OFFLINE_ITEM_ID);
    verify(this.cacheEvictItemService).evictFindItemByProductSku(CacheEvictHelperServiceImplTest.STORE_ID,
        CacheEvictHelperServiceImplTest.PRODUCT_SKU);
    verify(this.cacheEvictItemService).evictFindL5ByItemSku(CacheEvictHelperServiceImplTest.STORE_ID,
        CacheEvictHelperServiceImplTest.ITEM_SKU);
    verify(this.cacheEvictItemService).evictFindAllActivePromoBundlingsByItemCode(STORE_ID, ITEM_CODE, PICKUP_POINT_CODE);
    verify(this.cacheEvictItemService).evictFindAllActivePromoBundlingsByPristineId(STORE_ID, PRISTINE_ID, PICKUP_POINT_CODE);
  }

  @Test
  public void evictItemPickupPointCachePristineIdNullTest() {
    this.itemPickupPoint = new ItemPickupPoint();
    itemPickupPoint.setItemSku(ITEM_SKU);
    itemPickupPoint.setProductSku(PRODUCT_SKU);
    itemPickupPoint.setPickupPointCode(PICKUP_POINT_CODE);
    itemPickupPoint.setOfflineItemId(OFFLINE_ITEM_ID);
    item.setPristineDataItem(null);
    when(itemService.findByStoreIdAndItemSkus(STORE_ID, new HashSet<>(Arrays.asList(ITEM_SKU)))).thenReturn(Arrays.asList(item));
    this.cacheEvictHelperServiceImpl.evictItemPickupPointCache(CacheEvictHelperServiceImplTest.STORE_ID, null,
        Collections.singletonList(itemPickupPoint));
    verify(this.cacheEvictItemService).evictFindItemPickupPointByItemSkuAndPickupPointCode(
        CacheEvictHelperServiceImplTest.STORE_ID, CacheEvictHelperServiceImplTest.ITEM_SKU,
        itemPickupPoint.getPickupPointCode());
    verify(this.cacheEvictItemService).evictUniqueIdTypeCheck(CacheEvictHelperServiceImplTest.STORE_ID,
        OFFLINE_ITEM_ID);
    verify(this.cacheEvictItemService).evictFindItemByProductSku(CacheEvictHelperServiceImplTest.STORE_ID,
        CacheEvictHelperServiceImplTest.PRODUCT_SKU);
    verify(this.cacheEvictItemService).evictFindL5ByItemSku(CacheEvictHelperServiceImplTest.STORE_ID,
        CacheEvictHelperServiceImplTest.ITEM_SKU);
    verify(this.cacheEvictItemService).evictFindAllActivePromoBundlingsByItemCode(STORE_ID, ITEM_CODE, PICKUP_POINT_CODE);
  }

  @Test
  public void evictItemPickupPointCacheEmptyListTest() {
    this.cacheEvictHelperServiceImpl.evictItemPickupPointCache(CacheEvictHelperServiceImplTest.STORE_ID, null,
        new ArrayList<>());
  }

  @AfterEach
  public void tearDown() throws Exception {
    verifyNoMoreInteractions(this.cacheEvictItemService);
    verifyNoMoreInteractions(this.cacheEvictProductService, this.pristineCacheableService);
  }

  @Test
  public void evictProductDataRedisTemplateRemovedTest() {
    ReflectionTestUtils.setField(cacheEvictHelperServiceImpl,"redisTemplateRemoved",true);
    Product product = new Product(CacheEvictHelperServiceImplTest.PRODUCT_SKU);
    product.setMerchantCode(CacheEvictHelperServiceImplTest.MERCHANT_CODE);
    product.setProductCode(CacheEvictHelperServiceImplTest.PRODUCT_CODE);
    this.cacheEvictHelperServiceImpl.evictProductData(CacheEvictHelperServiceImplTest.STORE_ID,
        product);
    verify(this.cacheEvictProductService).evictFindProductByProductCodeUsingCacheEvict(
        CacheEvictHelperServiceImplTest.STORE_ID, CacheEvictHelperServiceImplTest.PRODUCT_CODE);
    verify(this.cacheEvictProductService).evictFindProductByProductSku(
        CacheEvictHelperServiceImplTest.STORE_ID, CacheEvictHelperServiceImplTest.PRODUCT_SKU);
  }

  @Test
  public void evictItemPickupPointData() {
    ItemPickupPoint itemPickupPoint1 = new ItemPickupPoint();
    itemPickupPoint1.setStoreId(STORE_ID);
    itemPickupPoint1.setProductSku(PRODUCT_SKU);
    itemPickupPoint1.setItemSku(ITEM_SKU);
    itemPickupPoint1.setPickupPointCode(PICKUP_POINT_CODE);

    ItemPickupPoint itemPickupPoint2 = new ItemPickupPoint();
    itemPickupPoint2.setStoreId(STORE_ID);
    itemPickupPoint2.setProductSku(PRODUCT_SKU);
    itemPickupPoint2.setItemSku(ITEM_SKU1);
    itemPickupPoint2.setPickupPointCode(PICKUP_POINT_CODE);

    Item item1 = new Item();
    item1.setItemSku(ITEM_SKU);
    item1.setItemCode(ITEM_CODE);

    Item item2 = new Item();
    item2.setItemSku(ITEM_SKU1);
    item2.setItemCode(ITEM_CODE1);
    item2.setPristineDataItem(pristineDataItem);

    cacheEvictHelperServiceImpl.evictItemPickupPointData(List.of(itemPickupPoint1, itemPickupPoint2),
        List.of(item1, item2));

    Mockito.verify(this.cacheEvictItemService)
        .evictFindItemPickupPointByItemSkuAndPickupPointCode(STORE_ID, ITEM_SKU, PICKUP_POINT_CODE);
    Mockito.verify(this.cacheEvictItemService)
        .evictFindItemPickupPointByItemSkuAndPickupPointCode(STORE_ID, ITEM_SKU1, PICKUP_POINT_CODE);
    Mockito.verify(this.cacheEvictItemService).evictFindL5ByItemSku(STORE_ID, ITEM_SKU);
    Mockito.verify(this.cacheEvictItemService).evictFindL5ByItemSku(STORE_ID, ITEM_SKU1);
    Mockito.verify(this.cacheEvictItemService).evictFindItemByProductSku(STORE_ID, PRODUCT_SKU);
    Mockito.verify(this.cacheEvictItemService)
        .evictFindAllActivePromoBundlingsByItemCode(STORE_ID, ITEM_CODE, PICKUP_POINT_CODE);
    Mockito.verify(this.cacheEvictItemService)
        .evictFindAllActivePromoBundlingsByItemCode(STORE_ID, ITEM_CODE1, PICKUP_POINT_CODE);
    Mockito.verify(this.cacheEvictItemService)
        .evictFindAllActivePromoBundlingsByPristineId(STORE_ID, PRISTINE_ID, PICKUP_POINT_CODE);
  }
}
