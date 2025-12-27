package com.gdn.x.product.service.cache;

import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.apache.commons.lang3.StringUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.test.util.ReflectionTestUtils;

import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.x.product.exception.ApiIncorrectInputDataException;
import com.gdn.x.product.model.entity.Item;
import com.gdn.x.product.model.entity.ItemPickupPoint;
import com.gdn.x.product.model.entity.ItemViewConfig;
import com.gdn.x.product.model.entity.Price;
import com.gdn.x.product.model.entity.PristineDataItem;
import com.gdn.x.product.model.vo.ItemAndItemPickupPointVo;
import com.gdn.x.product.service.api.CacheItemHelperService;
import com.gdn.x.product.service.api.ItemPickupPointService;
import com.gdn.x.product.service.api.ItemPriceService;
import com.gdn.x.product.service.api.ItemService;
import com.gdn.x.product.service.api.ObjectConverterService;
import com.google.common.collect.ImmutableMap;
import com.google.common.collect.ImmutableSet;

/**
 * Created by govind on 27/02/2018 AD.
 */
public class ItemCacheableServiceImplTest {

  private static final String STORE_ID = "store-id";
  private static final String PRISTINE_ID = "pristine-id";
  private static final String REQUEST_ID = "request-id";
  private static final String USER_NAME = "username";
  private static final String ITEM_SKU = "itemSku";
  private static final String ITEM_SKU2 = "itemSku2";
  private static final String ITEM_CODE = "itemCode";
  private static final String ITEM = "item";
  private static final String PRISTINE_PRODUCT_NAME = "pristineProductName";
  private static final Double OFFER_PRICE = 10000.0;
  private static final Double LIST_PRICE = 10000.0;
  private static final String PRODUCTSKU = "productSku";
  private static final String L5_ID = "item-sku-pickup-point";
  private static final String PICK_UP_POINT_CODE = "pickupPointCode";
  private static final int PAGE = 1;
  private static final int PAGE_SIZE = 10;
  private static final long TOTAL_L5_COUNT = 1000;

  @InjectMocks
  private ItemCacheableServiceImpl itemCacheableServiceImpl;

  @Mock
  private CacheItemHelperService cacheItemHelperService;

  @Mock
  private ItemPriceService itemPriceService;

  @Mock
  private ItemService itemService;

  @Mock
  private ItemPickupPointService itemPickupPointService;

  @Mock
  private ObjectConverterService objectConverterService;

  private Item item1, item2;
  private Set<String> activePromoBundlings;
  private PristineDataItem pristineDataItem;
  private Price price;
  private ItemViewConfig itemViewConfig;
  private Page<ItemPickupPoint> itemPickupPointPage;
  private ItemPickupPoint itemPickupPoint;
  private Map<String, ItemPickupPoint> itemPickupPointMap;

  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.openMocks(this);

    this.activePromoBundlings = new HashSet<>();

    this.pristineDataItem = new PristineDataItem();
    this.pristineDataItem.setPristineProductName(PRISTINE_PRODUCT_NAME);
    this.pristineDataItem.setPristineId(PRISTINE_ID);

    this.itemViewConfig = new ItemViewConfig();
    this.itemViewConfig.setBuyable(true);

    this.price = new Price();
    this.price.setOfferPrice(OFFER_PRICE);
    this.price.setListPrice(LIST_PRICE);

    this.item1 = new Item();
    this.item1.setItemSku(ITEM_SKU);
    this.item1.setStoreId(STORE_ID);
    this.item1.setPristineDataItem(this.pristineDataItem);
    this.item1.setItemCode(ITEM_CODE);
    this.item1.setActivePromoBundlings(Collections.singleton("COMBO"));
    this.item1.setPrice(Collections.singleton(this.price));
    this.item1.setItemViewConfigs(Collections.singleton(this.itemViewConfig));
    this.item1.setSynchronized(true);

    this.item2 = new Item();
    this.item2.setItemSku(ITEM_SKU2);
    this.item2.setStoreId(STORE_ID);
    this.item2.setPristineDataItem(this.pristineDataItem);
    this.item2.setItemCode(ITEM_CODE);
    this.item2.setActivePromoBundlings(Collections.singleton("WHOLESALE"));
    this.item2.setPrice(Collections.singleton(this.price));
    this.item2.setItemViewConfigs(Collections.singleton(this.itemViewConfig));
    this.item2.setSynchronized(true);

    itemPickupPoint = ItemPickupPoint.builder().itemSku(ITEM_SKU).pickupPointCode(PICK_UP_POINT_CODE)
        .price(Collections.singleton(this.price)).itemViewConfig(Collections.singleton(this.itemViewConfig)).build();
    ItemPickupPoint itemPickupPoint2 =
        ItemPickupPoint.builder().itemSku(ITEM_SKU2).price(Collections.singleton(this.price))
            .itemViewConfig(Collections.singleton(this.itemViewConfig)).build();
    when(itemPickupPointService.findByStoreIdAndItemSkuAndPickupPointCodeAndCncActiveAndMarkForDeleteFalse(eq(STORE_ID),
        eq(ITEM_SKU), Mockito.anyString(), eq(true))).thenReturn(itemPickupPoint);
    when(itemPickupPointService.findByItemSkusAndDelivery(STORE_ID, Arrays.asList(ITEM_SKU), true)).thenReturn(
        Arrays.asList(itemPickupPoint));
    when(itemPickupPointService.findByItemSkusAndDelivery(STORE_ID, Arrays.asList(ITEM_SKU2), true)).thenReturn(
        Arrays.asList(itemPickupPoint2));
    when(itemPickupPointService.findByItemSkusAndDelivery(STORE_ID, Arrays.asList(ITEM_SKU, ITEM_SKU2), true)).thenReturn(
        Arrays.asList(itemPickupPoint, itemPickupPoint2));
    when(itemPickupPointService.findOneForEachItemSkuIn(STORE_ID, List.of(ITEM_SKU2))).thenReturn(
        Collections.singletonList(itemPickupPoint2));

    itemPickupPointMap = new HashMap<>();
    itemPickupPointMap.put(itemPickupPoint.getItemSku(), itemPickupPoint);
  }

  @AfterEach
  public void tearDown() throws Exception {
    Mockito.verifyNoMoreInteractions(cacheItemHelperService);
    Mockito.verifyNoMoreInteractions(itemService);
    Mockito.verifyNoMoreInteractions(objectConverterService);
    Mockito.verifyNoMoreInteractions(itemPickupPointService);
  }

  @Test
  public void findItemsByPristineId() throws Exception {
    Item item = new Item();
    item.setItemSku(ITEM_SKU);
    Set<String> itemSkus = new HashSet<>();
    itemSkus.add(ITEM_SKU);
    List<Item> result = new ArrayList<>();
    ItemViewConfig itemViewConfig = new ItemViewConfig();
    itemViewConfig.setBuyable(true);
    itemViewConfig.setDiscoverable(true);
    Set itemViewConfigs = new HashSet<>();
    itemViewConfigs.add(itemViewConfig);
    item.setItemViewConfigs(itemViewConfigs);
    result.add(item);
    Set<Price> priceSet = new HashSet<>();
    priceSet.add(new Price());
    Map<String, Set<Price>> prices = new HashMap<>();
    prices.put(ITEM_SKU, priceSet);
    Map<String, Item> itemSkuItemMap = new HashMap<>();
    itemSkuItemMap.put(ITEM_SKU, item);

    Mockito.when(this.cacheItemHelperService
        .findCacheableItemSkusByStoreIdAndPristineIdAndMarkForDeleteFalse(Mockito.eq(STORE_ID),
            Mockito.eq(PRISTINE_ID))).thenReturn(itemSkus);
    Mockito.when(cacheItemHelperService.getCacheableItemsByItemSkus(STORE_ID, itemSkus))
        .thenReturn(itemSkuItemMap);
    Mockito.when(itemService.isItemBuyableAndDiscoverable(Mockito.any(Item.class))).thenReturn(true);
    this.itemCacheableServiceImpl.findItemsByPristineId(STORE_ID, USER_NAME, REQUEST_ID, PRISTINE_ID, false);
    Mockito.verify(this.cacheItemHelperService)
        .findCacheableItemSkusByStoreIdAndPristineIdAndMarkForDeleteFalse(Mockito.eq(STORE_ID),
            Mockito.eq(PRISTINE_ID));
    Mockito.verify(cacheItemHelperService).getCacheableItemsByItemSkus(STORE_ID, itemSkus);
    for (Item itm: result) {
      Mockito.verify(itemService).isItemBuyableAndDiscoverable(itm);
      Mockito.verify(itemService).getItemsWithDiscountPrice(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyList());
    }
  }

  @Test
  public void
  findItemByStoreIdAndItemSkuAndMarkForDeleteFalse_successWithCombineOthersBundlingsTrueAndPristineDataItemExist() {
    this.activePromoBundlings.add("WHOLESALE");
    this.activePromoBundlings.add("COMBO");
    Mockito.when(cacheItemHelperService
        .findCacheableByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, ITEM_SKU)).thenReturn(this.item1);
    Mockito.when(cacheItemHelperService
        .findCacheableActivePromoBundlingsByStoreIdAndPristine(STORE_ID, this.pristineDataItem))
        .thenReturn(this.activePromoBundlings);
    Mockito.when(itemService.getItemPriceAndViewConfigs(STORE_ID, Arrays.asList(ITEM_SKU)))
        .thenReturn(Arrays.asList(this.item1));

    Item result = this.itemCacheableServiceImpl
        .findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, ITEM_SKU, true, true, false, null, false,
          false);

    Mockito.verify(cacheItemHelperService)
        .findCacheableByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, ITEM_SKU);
    Mockito.verify(cacheItemHelperService)
        .findCacheableActivePromoBundlingsByStoreIdAndPristine(STORE_ID, this.pristineDataItem);
    Mockito.verify(itemPickupPointService).findByItemSkuAndDelivery(STORE_ID, ITEM_SKU);
    Assertions.assertNotNull(result);
    Assertions.assertEquals(result.getActivePromoBundlings(), this.activePromoBundlings);
  }

  @Test
  public void
  findItemByStoreIdAndItemSku_Test() {
    this.activePromoBundlings.add("WHOLESALE");
    this.activePromoBundlings.add("COMBO");
    Mockito.when(cacheItemHelperService
        .findCacheableByStoreIdAndItemSku(STORE_ID, ITEM_SKU)).thenReturn(this.item1);
    Mockito.when(cacheItemHelperService
        .findCacheableActivePromoBundlingsByStoreIdAndPristine(STORE_ID, this.pristineDataItem))
        .thenReturn(this.activePromoBundlings);
    Mockito.when(itemService.getItemPriceAndViewConfigs(STORE_ID, Arrays.asList(ITEM_SKU)))
        .thenReturn(Arrays.asList(this.item1));
    Mockito.when(itemPickupPointService.findByItemSkuAndDelivery(Mockito.anyString(), Mockito.anyString()))
        .thenReturn(itemPickupPoint);

    Item result = this.itemCacheableServiceImpl
        .findItemByStoreIdAndItemSku(STORE_ID, ITEM_SKU, true, true, false, null, false);

    Mockito.verify(cacheItemHelperService)
        .findCacheableByStoreIdAndItemSku(STORE_ID, ITEM_SKU);
    Mockito.verify(cacheItemHelperService)
        .findCacheableActivePromoBundlingsByStoreIdAndPristine(STORE_ID, this.pristineDataItem);
    Mockito.verify(itemPickupPointService).findByItemSkuAndDelivery(STORE_ID, ITEM_SKU);
    Mockito.verify(itemPickupPointService).findByItemSkusAndDelivery(STORE_ID, Arrays.asList(ITEM_SKU), true);
    Mockito.verify(objectConverterService).overrideL4DetailsFromL5(Mockito.anyList(), Mockito.anyList());

    Assertions.assertNotNull(result);
    Assertions.assertEquals(result.getActivePromoBundlings(), this.activePromoBundlings);
  }

  @Test
  public void findItemByStoreIdAndItemSkuAndMarkForDeleteFalse_successWithCombineOthersBundlingsTrueAndItemCodeExists() {
    this.activePromoBundlings.add("COMBO");
    this.item1.setPristineDataItem(null);

    Mockito.when(cacheItemHelperService
        .findCacheableByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, ITEM_SKU)).thenReturn(this.item1);
    Mockito.when(cacheItemHelperService
        .findCacheableActivePromoBundlingsByStoreIdAndItemCode(STORE_ID, ITEM_CODE))
        .thenReturn(this.activePromoBundlings);
    Mockito.when(itemService.getItemPriceAndViewConfigs(STORE_ID, Arrays.asList(ITEM_SKU)))
        .thenReturn(Arrays.asList(this.item1));
    Mockito.when(itemPickupPointService.findByItemSkuAndDelivery(STORE_ID, ITEM_SKU)).thenReturn(new ItemPickupPoint());

    Item result = this.itemCacheableServiceImpl
        .findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, ITEM_SKU, true, true, false, null, false,
          false);

    Mockito.verify(cacheItemHelperService)
        .findCacheableByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, ITEM_SKU);
    Mockito.verify(cacheItemHelperService)
        .findCacheableActivePromoBundlingsByStoreIdAndItemCode(STORE_ID, ITEM_CODE);
    Mockito.verify(itemPickupPointService).findByItemSkuAndDelivery(STORE_ID, ITEM_SKU);
    Mockito.verify(itemPickupPointService).findByItemSkusAndDelivery(STORE_ID, Arrays.asList(ITEM_SKU), true);
    Mockito.verify(objectConverterService).overrideL4DetailsFromL5(Mockito.anyList(), Mockito.anyList());

    Assertions.assertNotNull(result);
    Assertions.assertEquals(result.getActivePromoBundlings(), this.activePromoBundlings);
  }

  @Test
  public void findItemByItemSkuAndPickupPointCodeAndMarkForDeleteFalse_successWithCombineOthersBundlingsTrueAndPristineDataItemExist() {
    this.activePromoBundlings.add("WHOLESALE");
    this.activePromoBundlings.add("COMBO");
    itemPickupPoint.setMarkForDelete(false);
    Mockito.when(cacheItemHelperService
        .findCacheableByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, ITEM_SKU)).thenReturn(this.item1);
    Mockito.when(cacheItemHelperService
        .findCacheableActivePromoBundlingsByStoreIdAndPristineAndPickupPointCode(STORE_ID, this.pristineDataItem, PICK_UP_POINT_CODE))
        .thenReturn(this.activePromoBundlings);
    Mockito.when(itemPickupPointService.findByItemSkuAndPickupPointCode(STORE_ID,
        ITEM_SKU, PICK_UP_POINT_CODE)).thenReturn(itemPickupPoint);

    ItemAndItemPickupPointVo result = this.itemCacheableServiceImpl
        .findItemByItemSkuAndPickupPointCodeAndMarkForDeleteFalse(STORE_ID, ITEM_SKU, PICK_UP_POINT_CODE);

    Mockito.verify(cacheItemHelperService)
        .findCacheableByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, ITEM_SKU);
    Mockito.verify(cacheItemHelperService)
        .findCacheableActivePromoBundlingsByStoreIdAndPristineAndPickupPointCode(STORE_ID, this.pristineDataItem, PICK_UP_POINT_CODE);
    Mockito.verify(itemPickupPointService)
        .findByItemSkuAndPickupPointCode(STORE_ID, ITEM_SKU, PICK_UP_POINT_CODE);
    Assertions.assertNotNull(result);
    Assertions.assertEquals(result.getItemPickupPoints().get(0).getActivePromoBundlings(), this.activePromoBundlings);
  }

  @Test
  public void findItemByItemSkuAndPickupPointCodeAndMarkForDeleteFalse_successWithCombineOthersBundlingsTrueAndPristineDataItemItemCodeExist() {
    this.activePromoBundlings.add("WHOLESALE");
    this.activePromoBundlings.add("COMBO");
    itemPickupPoint.setMarkForDelete(false);
    item1.setPristineDataItem(null);
    item1.setItemCode(ITEM_CODE);
    Mockito.when(cacheItemHelperService
        .findCacheableByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, ITEM_SKU)).thenReturn(this.item1);
    Mockito.when(cacheItemHelperService
        .findCacheableActivePromoBundlingsByStoreIdAndItemCodeAndPickupPointCode(STORE_ID, ITEM_CODE, PICK_UP_POINT_CODE))
        .thenReturn(this.activePromoBundlings);
    Mockito.when(itemPickupPointService.findByItemSkuAndPickupPointCode(STORE_ID,
        ITEM_SKU, PICK_UP_POINT_CODE)).thenReturn(itemPickupPoint);

    ItemAndItemPickupPointVo result = this.itemCacheableServiceImpl
        .findItemByItemSkuAndPickupPointCodeAndMarkForDeleteFalse(STORE_ID, ITEM_SKU, PICK_UP_POINT_CODE);

    Mockito.verify(cacheItemHelperService)
        .findCacheableByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, ITEM_SKU);
    Mockito.verify(cacheItemHelperService)
        .findCacheableActivePromoBundlingsByStoreIdAndItemCodeAndPickupPointCode(STORE_ID, ITEM_CODE, PICK_UP_POINT_CODE);
    Mockito.verify(itemPickupPointService)
        .findByItemSkuAndPickupPointCode(STORE_ID, ITEM_SKU, PICK_UP_POINT_CODE);
    Assertions.assertNotNull(result);
    Assertions.assertEquals(result.getItemPickupPoints().get(0).getActivePromoBundlings(), this.activePromoBundlings);
  }

  @Test
  public void findItemByItemSkuAndPickupPointCodeAndMarkForDeleteFalseNullObjectTest() {
    itemPickupPoint.setMarkForDelete(true);
    Mockito.when(cacheItemHelperService.findCacheableByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, ITEM_SKU))
        .thenReturn(this.item1);
    Mockito.when(itemPickupPointService.findByItemSkuAndPickupPointCode(STORE_ID, ITEM_SKU, PICK_UP_POINT_CODE))
        .thenReturn(itemPickupPoint);

    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.itemCacheableServiceImpl.findItemByItemSkuAndPickupPointCodeAndMarkForDeleteFalse(STORE_ID, ITEM_SKU,
              PICK_UP_POINT_CODE));
    } finally {
      Mockito.verify(cacheItemHelperService).findCacheableByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, ITEM_SKU);
      Mockito.verify(itemPickupPointService).findByItemSkuAndPickupPointCode(STORE_ID, ITEM_SKU, PICK_UP_POINT_CODE);
    }
  }

  @Test
  public void findItemByItemSkuAndPickupPointCodeAndMarkForDeleteFalseNullItemTest() {
    Mockito.when(cacheItemHelperService.findCacheableByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, ITEM_SKU))
        .thenReturn(null);
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () ->
          this.itemCacheableServiceImpl.findItemByItemSkuAndPickupPointCodeAndMarkForDeleteFalse(STORE_ID, ITEM_SKU,
              PICK_UP_POINT_CODE));
    } finally {
      Mockito.verify(cacheItemHelperService).findCacheableByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, ITEM_SKU);
    }
  }

  @Test
  public void findItemByItemSkuAndPickupPointCodeAndMarkForDeleteFalseArchivedTest() {
    item1.setArchived(true);
    Mockito.when(cacheItemHelperService.findCacheableByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, ITEM_SKU))
        .thenReturn(item1);
    try {
      Assertions.assertThrows(ApiIncorrectInputDataException.class, () -> this.itemCacheableServiceImpl.findItemByItemSkuAndPickupPointCodeAndMarkForDeleteFalse(STORE_ID, ITEM_SKU,
              PICK_UP_POINT_CODE));
    } finally {
      Mockito.verify(cacheItemHelperService).findCacheableByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, ITEM_SKU);
    }
  }

  @Test
  public void findItemByItemSkuAndPickupPointCodeAndMarkForDeleteFalseMFDObjectTest() {
    itemPickupPoint.setMarkForDelete(true);
    Mockito.when(cacheItemHelperService.findCacheableByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, ITEM_SKU))
        .thenReturn(this.item1);
    Mockito.when(itemPickupPointService.findByItemSkuAndPickupPointCode(STORE_ID, ITEM_SKU, PICK_UP_POINT_CODE))
        .thenReturn(null);

    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () ->
          this.itemCacheableServiceImpl.findItemByItemSkuAndPickupPointCodeAndMarkForDeleteFalse(STORE_ID, ITEM_SKU,
              PICK_UP_POINT_CODE));
    } finally {
      Mockito.verify(cacheItemHelperService).findCacheableByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, ITEM_SKU);
      Mockito.verify(itemPickupPointService).findByItemSkuAndPickupPointCode(STORE_ID, ITEM_SKU, PICK_UP_POINT_CODE);
    }
  }

  @Test
  public void findItemByItemSkuAndPickupPointCodeAndMarkForDeleteFalse_successExist() {
    item1.setSynchronized(false);
    itemPickupPoint.setMarkForDelete(false);
    Mockito.when(cacheItemHelperService
        .findCacheableByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, ITEM_SKU)).thenReturn(this.item1);
    Mockito.when(itemPickupPointService.findByItemSkuAndPickupPointCode(STORE_ID,
        ITEM_SKU, PICK_UP_POINT_CODE)).thenReturn(itemPickupPoint);

    ItemAndItemPickupPointVo result = this.itemCacheableServiceImpl
        .findItemByItemSkuAndPickupPointCodeAndMarkForDeleteFalse(STORE_ID, ITEM_SKU, PICK_UP_POINT_CODE);

    Mockito.verify(cacheItemHelperService)
        .findCacheableByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, ITEM_SKU);
    Mockito.verify(itemPickupPointService)
        .findByItemSkuAndPickupPointCode(STORE_ID, ITEM_SKU, PICK_UP_POINT_CODE);
    Assertions.assertNotNull(result);
  }

  @Test
  public void findItemAndItemPickPointByproductSkusTest() {
    Item item = new Item();
    item.setItemSku(ITEM_SKU);
    Set<String> itemSkus = new HashSet<>();
    itemSkus.add(ITEM_SKU);
    this.activePromoBundlings.add("WHOLESALE");
    this.activePromoBundlings.add("COMBO");
    itemPickupPointPage =  new PageImpl<>(Arrays.asList(itemPickupPoint), PageRequest.of(PAGE,
      PAGE_SIZE), TOTAL_L5_COUNT);
    Mockito.when(itemPickupPointService.findItemPickupPointByProductSkusOrItemSkus(STORE_ID,
        Arrays.asList(PRODUCTSKU), Arrays.asList(ITEM_SKU), PAGE, PAGE_SIZE)).thenReturn(itemPickupPointPage);
    Mockito.when(itemService.findItemsByStoreIdAndProductSkuInOrItemSkuInAndMarkForDeleteFalse(STORE_ID,
        Arrays.asList(PRODUCTSKU), Arrays.asList(ITEM_SKU))).thenReturn(Arrays.asList(item));
    ItemAndItemPickupPointVo result = this.itemCacheableServiceImpl
        .findItemAndItemPickPointByproductSkus(STORE_ID, Arrays.asList(PRODUCTSKU), Arrays.asList(ITEM_SKU), false, PAGE, PAGE_SIZE);

    Mockito.verify(itemPickupPointService).findItemPickupPointByProductSkusOrItemSkus(STORE_ID,
            Arrays.asList(PRODUCTSKU), Arrays.asList(ITEM_SKU), PAGE, PAGE_SIZE);
    Mockito.verify(itemService)
        .findItemsByStoreIdAndProductSkuInOrItemSkuInAndMarkForDeleteFalse(STORE_ID, Arrays.asList(PRODUCTSKU),
            Arrays.asList(ITEM_SKU));
    Assertions.assertNotNull(result);
    Assertions.assertEquals(TOTAL_L5_COUNT, result.getTotalL5Count());
  }

  @Test
  public void findItemAndItemPickPointByproductSkusWithShowDeletedTrueTest() {
    Item item = new Item();
    item.setItemSku(ITEM_SKU);
    Set<String> itemSkus = new HashSet<>();
    itemSkus.add(ITEM_SKU);
    this.activePromoBundlings.add("WHOLESALE");
    this.activePromoBundlings.add("COMBO");
    itemPickupPointPage =  new PageImpl<>(Arrays.asList(itemPickupPoint), PageRequest.of(PAGE,
        PAGE_SIZE), TOTAL_L5_COUNT);
    Mockito.when(itemPickupPointService.findAllItemPickupPointByProductSkusOrItemSkus(STORE_ID,
        Arrays.asList(PRODUCTSKU), Arrays.asList(ITEM_SKU), PAGE, PAGE_SIZE)).thenReturn(itemPickupPointPage);
    Mockito.when(itemService.getItemsByStoreIdAndProductSkusOrItemSkusIn(STORE_ID, Arrays.asList(PRODUCTSKU),
        Arrays.asList(ITEM_SKU))).thenReturn(Arrays.asList(item));
    ItemAndItemPickupPointVo result = this.itemCacheableServiceImpl
        .findItemAndItemPickPointByproductSkus(STORE_ID, Arrays.asList(PRODUCTSKU), Arrays.asList(ITEM_SKU), true, PAGE, PAGE_SIZE);

    Mockito.verify(itemPickupPointService).findAllItemPickupPointByProductSkusOrItemSkus(STORE_ID,
        Arrays.asList(PRODUCTSKU), Arrays.asList(ITEM_SKU), PAGE, PAGE_SIZE);
    Mockito.verify(itemService)
        .getItemsByStoreIdAndProductSkusOrItemSkusIn(STORE_ID, Arrays.asList(PRODUCTSKU), Arrays.asList(ITEM_SKU));
    Assertions.assertNotNull(result);
  }

  @Test
  public void findItemAndItemPickPointByproductSkusContainsItemTest() {
    Item item = new Item();
    item.setItemSku(ITEM_SKU);
    Set<String> itemSkus = new HashSet<>();
    itemSkus.add(ITEM_SKU);
    this.activePromoBundlings.add("WHOLESALE");
    this.activePromoBundlings.add("COMBO");
    itemPickupPointPage =  new PageImpl<>(Arrays.asList(itemPickupPoint), PageRequest.of(PAGE,
        PAGE_SIZE), TOTAL_L5_COUNT);
    Mockito.when(itemPickupPointService.findItemPickupPointByProductSkusOrItemSkus(STORE_ID,
        Arrays.asList(PRODUCTSKU), Arrays.asList(ITEM_SKU), PAGE, PAGE_SIZE)).thenReturn(itemPickupPointPage);
    Mockito.when(itemService.findItemsByStoreIdAndProductSkuInOrItemSkuInAndMarkForDeleteFalse(STORE_ID,
        Arrays.asList(PRODUCTSKU), Arrays.asList(ITEM_SKU))).thenReturn(Arrays.asList(item));
    ItemAndItemPickupPointVo result = this.itemCacheableServiceImpl
        .findItemAndItemPickPointByproductSkus(STORE_ID, Arrays.asList(PRODUCTSKU), Arrays.asList(ITEM_SKU), false, PAGE, PAGE_SIZE);

    Mockito.verify(itemPickupPointService).findItemPickupPointByProductSkusOrItemSkus(STORE_ID,
        Arrays.asList(PRODUCTSKU), Arrays.asList(ITEM_SKU), PAGE, PAGE_SIZE);
    Mockito.verify(itemService)
        .findItemsByStoreIdAndProductSkuInOrItemSkuInAndMarkForDeleteFalse(STORE_ID, Arrays.asList(PRODUCTSKU),
            Arrays.asList(ITEM_SKU));
    Assertions.assertNotNull(result);
    Assertions.assertEquals(TOTAL_L5_COUNT, result.getTotalL5Count());
  }

  @Test
  public void findItemAndItemPickPointByproductSkusL4MfdTrueTest() {
    Item item = new Item();
    item.setItemSku(ITEM_SKU);
    Set<String> itemSkus = new HashSet<>();
    itemSkus.add(ITEM_SKU);
    this.activePromoBundlings.add("WHOLESALE");
    this.activePromoBundlings.add("COMBO");
    itemPickupPointPage =  new PageImpl<>(Arrays.asList(itemPickupPoint), PageRequest.of(PAGE,
        PAGE_SIZE), TOTAL_L5_COUNT);
    Mockito.when(itemPickupPointService.findItemPickupPointByProductSkusOrItemSkus(STORE_ID,
        Arrays.asList(PRODUCTSKU), Arrays.asList(ITEM_SKU), PAGE, PAGE_SIZE)).thenReturn(itemPickupPointPage);
    Mockito.when(itemService.findItemsByStoreIdAndProductSkuInOrItemSkuInAndMarkForDeleteFalse(STORE_ID,
        Arrays.asList(PRODUCTSKU), Arrays.asList(ITEM_SKU))).thenReturn(Arrays.asList(item));
    ItemAndItemPickupPointVo result =
        this.itemCacheableServiceImpl.findItemAndItemPickPointByproductSkus(STORE_ID, Arrays.asList(PRODUCTSKU),
            Arrays.asList(ITEM_SKU), false, PAGE, PAGE_SIZE);
    Mockito.verify(itemPickupPointService).findItemPickupPointByProductSkusOrItemSkus(STORE_ID,
        Arrays.asList(PRODUCTSKU), Arrays.asList(ITEM_SKU), PAGE, PAGE_SIZE);
    Mockito.verify(itemService)
        .findItemsByStoreIdAndProductSkuInOrItemSkuInAndMarkForDeleteFalse(STORE_ID, Arrays.asList(PRODUCTSKU),
            Arrays.asList(ITEM_SKU));
    Assertions.assertNotNull(result);
    Assertions.assertEquals(TOTAL_L5_COUNT, result.getTotalL5Count());
  }

  @Test
  public void findItemByStoreIdAndItemSku_successWithCombineOthersBundlingsTrueAndItemCodeExists() {
    this.activePromoBundlings.add("COMBO");
    this.item1.setPristineDataItem(null);

    Mockito.when(cacheItemHelperService.findCacheableByStoreIdAndItemSku(STORE_ID, ITEM_SKU)).thenReturn(this.item1);
    Mockito.when(cacheItemHelperService.findCacheableActivePromoBundlingsByStoreIdAndItemCode(STORE_ID, ITEM_CODE))
        .thenReturn(this.activePromoBundlings);
    Mockito.when(itemService.getItemPriceAndViewConfigs(STORE_ID, Arrays.asList(ITEM_SKU)))
        .thenReturn(Arrays.asList(this.item1));
    Mockito.when(itemPickupPointService.findByItemSkuAndDelivery(Mockito.anyString(), Mockito.anyString()))
        .thenReturn(itemPickupPoint);

    Item result =
        this.itemCacheableServiceImpl.findItemByStoreIdAndItemSku(STORE_ID, ITEM_SKU, true, true, false, null, false);

    Mockito.verify(cacheItemHelperService).findCacheableByStoreIdAndItemSku(STORE_ID, ITEM_SKU);
    Mockito.verify(cacheItemHelperService).findCacheableActivePromoBundlingsByStoreIdAndItemCode(STORE_ID, ITEM_CODE);
    Mockito.verify(itemPickupPointService).findByItemSkuAndDelivery(STORE_ID, ITEM_SKU);
    Mockito.verify(itemPickupPointService).findByItemSkusAndDelivery(STORE_ID, Arrays.asList(ITEM_SKU), true);
    Mockito.verify(objectConverterService).overrideL4DetailsFromL5(Mockito.anyList(), Mockito.anyList());

    Assertions.assertNotNull(result);
    Assertions.assertEquals(result.getActivePromoBundlings(), this.activePromoBundlings);
  }

  @Test
  public void findItemByStoreIdAndItemSkuAndMarkForDeleteFalse_WithOff2OnTrue() {
    this.activePromoBundlings.add("WHOLESALE");
    this.activePromoBundlings.add("COMBO");
    this.item1.setOff2OnChannelActive(false);
    Mockito.when(cacheItemHelperService.findCacheableByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, ITEM_SKU))
        .thenReturn(this.item1);
    Item result = this.itemCacheableServiceImpl
        .findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, ITEM_SKU, true, true, false, null, true,
          false);
    Mockito.verify(cacheItemHelperService).findCacheableByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, ITEM_SKU);
    Assertions.assertNull(result);
  }

  @Test
  public void findItemByStoreIdAndItemSku_WithOff2OnTrue() {
    this.activePromoBundlings.add("WHOLESALE");
    this.activePromoBundlings.add("COMBO");
    this.item1.setOff2OnChannelActive(false);
    Mockito.when(cacheItemHelperService.findCacheableByStoreIdAndItemSku(STORE_ID, ITEM_SKU)).thenReturn(this.item1);
    Item result =
        this.itemCacheableServiceImpl.findItemByStoreIdAndItemSku(STORE_ID, ITEM_SKU, true, true, false, null, true);
    Mockito.verify(cacheItemHelperService).findCacheableByStoreIdAndItemSku(STORE_ID, ITEM_SKU);
    Assertions.assertNull(result);
  }

  @Test
  public void findAllItemByStoreIdAndItemCodeAndMarkForDeleteFalseTest() {
    Set<String> itemSkus = new HashSet<>();
    itemSkus.add(item1.getItemSku());
    Map itemSkuItemMap = new HashMap();
    itemSkuItemMap.put(item1.getItemSku(), item1);
    Mockito.when(cacheItemHelperService
        .findItemSkusByStoreIdAndItemCodeAndMarkForDeleteFalse(STORE_ID, ITEM_CODE))
        .thenReturn(itemSkus);
    Mockito.when(cacheItemHelperService.getCacheableItemsByItemSkus(STORE_ID, itemSkus))
        .thenReturn(itemSkuItemMap);
    List<Item> items = itemCacheableServiceImpl
        .findAllItemByStoreIdAndItemCodeAndMarkForDeleteFalse(STORE_ID, ITEM_CODE);
    Mockito.verify(cacheItemHelperService)
        .findItemSkusByStoreIdAndItemCodeAndMarkForDeleteFalse(STORE_ID, ITEM_CODE);
    Assertions.assertEquals(1, items.size());
    Mockito.verify(cacheItemHelperService).getCacheableItemsByItemSkus(STORE_ID, itemSkus);
    Assertions.assertEquals(ITEM_CODE, items.get(0).getItemCode());
    Assertions.assertEquals(ITEM_SKU, items.get(0).getItemSku());
  }

  @Test
  public void findAllItemByStoreIdAndItemCodeAndMarkForDeleteFalse_whenItemNotExistInCacheTest() {
    Set<String> itemSkus = new HashSet<>();
    itemSkus.add(item1.getItemSku());
    Map itemSkuItemMap = new HashMap();
    itemSkuItemMap.put(item1.getItemSku(), item1);
    Mockito.when(cacheItemHelperService
        .findItemSkusByStoreIdAndItemCodeAndMarkForDeleteFalse(STORE_ID, ITEM_CODE))
        .thenReturn(itemSkus);
    Mockito.when(cacheItemHelperService.getCacheableItemsByItemSkus(STORE_ID, itemSkus))
        .thenReturn(Collections.EMPTY_MAP);
    Mockito.when(cacheItemHelperService
        .findCacheableByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, item1.getItemSku()))
        .thenReturn(item1);
    List<Item> items = itemCacheableServiceImpl
        .findAllItemByStoreIdAndItemCodeAndMarkForDeleteFalse(STORE_ID, ITEM_CODE);
    Mockito.verify(cacheItemHelperService)
        .findItemSkusByStoreIdAndItemCodeAndMarkForDeleteFalse(STORE_ID, ITEM_CODE);
    Mockito.verify(cacheItemHelperService).getCacheableItemsByItemSkus(STORE_ID, itemSkus);
    Mockito.verify(cacheItemHelperService)
        .findCacheableByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, item1.getItemSku());
    Mockito.verify(itemPickupPointService).findByItemSkuAndDelivery(STORE_ID, ITEM_SKU);
    Assertions.assertEquals(1, items.size());
    Assertions.assertEquals(ITEM_CODE, items.get(0).getItemCode());
    Assertions.assertEquals(ITEM_SKU, items.get(0).getItemSku());
  }

  @Test
  public void findItemsByStoreIdAndItemSkusAndMarkForDeleteFalse() {
    Set<String> itemSkus = new HashSet<>();
    itemSkus.add(ITEM_SKU);
    itemCacheableServiceImpl.findItemsByStoreIdAndItemSkusAndMarkForDeleteFalse(STORE_ID, itemSkus);
    verify(cacheItemHelperService).getCacheableItemsByItemSkus(STORE_ID, itemSkus);
  }

  @Test
  public void getCacheableItemsByProductSkusTest(){
    Set<String> productSkus = new HashSet<>();
    productSkus.add(PRODUCTSKU);
    List<Item> items = new ArrayList<>();
    items.add(item1);
    items.add(item2);
    Mockito.when(this.cacheItemHelperService.getCacheableItemsByProductSkus(STORE_ID, productSkus))
        .thenReturn(items);
    List<Item> result = itemCacheableServiceImpl.getCacheableItemsByProductSkus(STORE_ID, productSkus);
    Mockito.verify(this.cacheItemHelperService)
        .getCacheableItemsByProductSkus(STORE_ID, productSkus);
    Assertions.assertEquals(items, result);
  }

  @Test
  public void findAllItemSkusByStoreIdAndItemCodeAndMarkForDeleteFalseTest(){
    Set<String> itemSkus = new HashSet<>();
    itemSkus.add(ITEM_SKU);
    Mockito.when(cacheItemHelperService
        .findItemSkusByStoreIdAndItemCodeAndMarkForDeleteFalse(STORE_ID, ITEM_CODE))
        .thenReturn(itemSkus);
    itemCacheableServiceImpl
        .findAllItemSkusByStoreIdAndItemCodeAndMarkForDeleteFalse(STORE_ID, ITEM_CODE);
    Mockito.verify(cacheItemHelperService)
        .findItemSkusByStoreIdAndItemCodeAndMarkForDeleteFalse(STORE_ID, ITEM_CODE);

  }

  @Test
  public void findItemsByStoreIdAndProductSku_withOff2OnFalseAndCombineOtherBundlingsFalseTest() {
    List<Item> items = new ArrayList<>();
    this.item2.setOff2OnChannelActive(true);
    items.add(item1);
    items.add(item2);
    Mockito.when(cacheItemHelperService.findCacheableByStoreIdAndProductSku(STORE_ID, PRODUCTSKU)).thenReturn(items);
    Mockito.when(itemService.getItemPriceAndViewConfigs(STORE_ID, Arrays.asList(ITEM_SKU, ITEM_SKU2)))
        .thenReturn(Arrays.asList(item1, item2));
    List<Item> result = itemCacheableServiceImpl.findItemsByStoreIdAndProductSku(STORE_ID, PRODUCTSKU, false, false,
        false);
    Mockito.verify(cacheItemHelperService).findCacheableByStoreIdAndProductSku(STORE_ID, PRODUCTSKU);
    Mockito.verify(itemPickupPointService).findByItemSkusAndDelivery(STORE_ID, Arrays.asList(ITEM_SKU, ITEM_SKU2), true);
    Assertions.assertEquals(2, result.size());
    Assertions.assertFalse(result.get(0).isOff2OnChannelActive());
    Assertions.assertTrue(result.get(1).isOff2OnChannelActive());
  }

  @Test
  public void findItemsByStoreIdAndProductSku_withOff2OnTrueAndCombineOtherBundlingsFalseTest() {
    List<Item> items = new ArrayList<>();
    this.item2.setOff2OnChannelActive(true);
    items.add(item1);
    items.add(item2);
    Mockito.when(cacheItemHelperService.findCacheableByStoreIdAndProductSku(STORE_ID, PRODUCTSKU)).thenReturn(items);
    List<Item> result = itemCacheableServiceImpl.findItemsByStoreIdAndProductSku(STORE_ID, PRODUCTSKU, false, true,
        false);
    Mockito.verify(cacheItemHelperService).findCacheableByStoreIdAndProductSku(STORE_ID, PRODUCTSKU);
    Mockito.verify(itemPickupPointService)
        .findByItemSkusAndDelivery(STORE_ID, Arrays.asList(ITEM_SKU2), true);
    Assertions.assertEquals(1, result.size());
    Assertions.assertTrue(result.get(0).isOff2OnChannelActive());
  }


  @Test
  public void findItemsByStoreIdAndProductSku_withOff2OnTrueAndCombineOtherBundlingsTrueTest() {
    List<Item> items = new ArrayList<>();
    this.item2.setOff2OnChannelActive(true);
    items.add(item1);
    items.add(item2);
    Mockito.when(cacheItemHelperService.findCacheableByStoreIdAndProductSku(STORE_ID, PRODUCTSKU)).thenReturn(items);
    Mockito.when(itemService.getItemPriceAndViewConfigs(STORE_ID, Arrays.asList(ITEM_SKU2)))
        .thenReturn(Arrays.asList(item2));
    Mockito.when(
        cacheItemHelperService.findCacheableActivePromoBundlingsByStoreIdAndPristine(STORE_ID, this.pristineDataItem))
        .thenReturn(this.activePromoBundlings);
    List<Item> result = itemCacheableServiceImpl.findItemsByStoreIdAndProductSku(STORE_ID, PRODUCTSKU, true, true,
        false);
    Mockito.verify(cacheItemHelperService).findCacheableByStoreIdAndProductSku(STORE_ID, PRODUCTSKU);
    Mockito.verify(itemPickupPointService).findByItemSkusAndDelivery(STORE_ID, Arrays.asList(ITEM_SKU2), true);
    Mockito.verify(cacheItemHelperService)
        .findCacheableActivePromoBundlingsByStoreIdAndPristine(STORE_ID, this.pristineDataItem);
    Assertions.assertEquals(1, result.size());
    Assertions.assertTrue(result.get(0).isOff2OnChannelActive());
  }


  @Test
  public void findItemsByStoreIdAndProductSkuAndMarkForDeleteFalse_withOff2OnFalseAndCombineOtherBundlingsFalseTest() {
    List<Item> items = new ArrayList<>();
    this.item2.setOff2OnChannelActive(true);
    items.add(item1);
    items.add(item2);
    Mockito.when(cacheItemHelperService.findCacheableByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCTSKU))
        .thenReturn(items);
    Mockito.when(itemService.getItemPriceAndViewConfigs(STORE_ID, Arrays.asList(ITEM_SKU, ITEM_SKU2)))
        .thenReturn(Arrays.asList(item1, item2));
    List<Item> result = itemCacheableServiceImpl
        .findItemsByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCTSKU, false, false,
          false);
    Mockito.verify(cacheItemHelperService)
        .findCacheableByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCTSKU);
    Mockito.verify(itemPickupPointService).findByItemSkusAndDelivery(STORE_ID, Arrays.asList(ITEM_SKU, ITEM_SKU2), true);
    Assertions.assertEquals(2, result.size());
    Assertions.assertFalse(result.get(0).isOff2OnChannelActive());
    Assertions.assertTrue(result.get(1).isOff2OnChannelActive());
  }


  @Test
  public void findItemsByStoreIdAndProductSkuAndMarkForDeleteFalse_withOff2OnTrueAndCombineOtherBundlingsFalseTest() {
    List<Item> items = new ArrayList<>();
    this.item2.setOff2OnChannelActive(true);
    items.add(item1);
    items.add(item2);
    Mockito.when(cacheItemHelperService.findCacheableByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCTSKU))
        .thenReturn(items);
    Mockito.when(itemService.getItemPriceAndViewConfigs(STORE_ID, Arrays.asList(ITEM_SKU2)))
        .thenReturn(Arrays.asList(item2));
    List<Item> result = itemCacheableServiceImpl
        .findItemsByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCTSKU, false, true,
          false);
    Mockito.verify(cacheItemHelperService)
        .findCacheableByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCTSKU);
    Mockito.verify(itemPickupPointService).findByItemSkusAndDelivery(STORE_ID, Arrays.asList(ITEM_SKU2), true);
    Assertions.assertEquals(1, result.size());
    Assertions.assertTrue(result.get(0).isOff2OnChannelActive());
  }

  @Test
  public void findItemsByStoreIdAndProductSkuAndShowDeletedFlagTest() {
    List<Item> items = new ArrayList<>();
    this.item1.setOff2OnChannelActive(true);
    items.add(item1);
    items.add(item2);
    Mockito.when(cacheItemHelperService.findCacheableByStoreIdAndProductSku(STORE_ID, PRODUCTSKU)).thenReturn(items);
    Mockito.when(itemService.getItemPriceAndViewConfigs(STORE_ID, Arrays.asList(ITEM_SKU2)))
        .thenReturn(Arrays.asList(item2));
    Mockito.when(cacheItemHelperService.findCacheableActivePromoBundlingsByStoreIdAndPristineAndPickupPointCode(STORE_ID,
        this.pristineDataItem, PICK_UP_POINT_CODE)).thenReturn(this.activePromoBundlings);
    List<Item> result = itemCacheableServiceImpl.findItemsByStoreIdAndProductSkuAndShowDeletedFlag(STORE_ID, PRODUCTSKU, true, true, true, itemPickupPointMap);
    Mockito.verify(cacheItemHelperService).findCacheableByStoreIdAndProductSku(STORE_ID, PRODUCTSKU);
    Mockito.verify(cacheItemHelperService)
        .findCacheableActivePromoBundlingsByStoreIdAndPristineAndPickupPointCode(STORE_ID, this.pristineDataItem, PICK_UP_POINT_CODE);
    Assertions.assertEquals(1, result.size());
    Assertions.assertTrue(result.get(0).isOff2OnChannelActive());
  }

  @Test
  public void findItemsByStoreIdAndProductSkuAndShowDeletedFlagFalseTest() {
    List<Item> items = new ArrayList<>();
    items.add(item1);
    items.add(item2);
    Mockito.when(cacheItemHelperService.findCacheableByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCTSKU)).thenReturn(items);
    List<Item> result = itemCacheableServiceImpl.findItemsByStoreIdAndProductSkuAndShowDeletedFlag(STORE_ID, PRODUCTSKU, false, false, false, itemPickupPointMap);
    Mockito.verify(cacheItemHelperService).findCacheableByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCTSKU);
    Assertions.assertEquals(1, result.size());
  }

  @Test
  public void findItemsByStoreIdAndProductSkuAndMarkForDeleteFalse_withOff2OnTrueAndCombineOtherBundlingsTrueTest() {
    List<Item> items = new ArrayList<>();
    this.item2.setOff2OnChannelActive(true);
    items.add(item1);
    items.add(item2);
    Mockito.when(cacheItemHelperService.findCacheableByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCTSKU))
        .thenReturn(items);
    Mockito.when(itemService.getItemPriceAndViewConfigs(STORE_ID, Arrays.asList(ITEM_SKU2)))
        .thenReturn(Arrays.asList(item2));
    Mockito.when(
        cacheItemHelperService.findCacheableActivePromoBundlingsByStoreIdAndPristine(STORE_ID, this.pristineDataItem))
        .thenReturn(this.activePromoBundlings);
    List<Item> result =
        itemCacheableServiceImpl.findItemsByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCTSKU, true, true,
          false);
    Mockito.verify(cacheItemHelperService)
        .findCacheableByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCTSKU);
    Mockito.verify(itemPickupPointService).findByItemSkusAndDelivery(STORE_ID, Arrays.asList(ITEM_SKU2), true);
    Mockito.verify(cacheItemHelperService)
        .findCacheableActivePromoBundlingsByStoreIdAndPristine(STORE_ID, this.pristineDataItem);
    Assertions.assertEquals(1, result.size());
    Assertions.assertTrue(result.get(0).isOff2OnChannelActive());
  }

  @Test
  public void findItemsByStoreIdAndProductSkuAndCncActivatedAndMarkForDeleteFalseTest() {
    List<Item> items = new ArrayList<>();
    this.item2.setOff2OnChannelActive(true);
    items.add(item1);
    items.add(item2);
    Mockito.when(cacheItemHelperService
        .findCacheableByStoreIdAndProductSkuAndCncActivatedAndMarkForDeleteFalse(STORE_ID, PRODUCTSKU, false))
        .thenReturn(items);
    Mockito.when(itemService.getItemPriceAndViewConfigs(STORE_ID, Arrays.asList(ITEM_SKU, ITEM_SKU2)))
        .thenReturn(Arrays.asList(item1, item2));
    List<Item> result = this.itemCacheableServiceImpl
        .findItemsByStoreIdAndProductSkuAndCncActivatedAndMarkForDeleteFalse(STORE_ID, PRODUCTSKU, false);
    Mockito.verify(cacheItemHelperService)
        .findCacheableByStoreIdAndProductSkuAndCncActivatedAndMarkForDeleteFalse(STORE_ID, PRODUCTSKU, false);
    Mockito.verify(itemPickupPointService)
        .findByItemSkusAndDelivery(STORE_ID, Arrays.asList(ITEM_SKU, ITEM_SKU2), true);
    Assertions.assertEquals(2, result.size());
  }

  @Test
  public void findItemCodesByPristineTest() {
    PristineDataItem pristineDataItem = new PristineDataItem();
    Mockito.when(cacheItemHelperService.findItemCodesByPristine(STORE_ID, pristineDataItem))
        .thenReturn(new HashSet<>());
    Set<String> result = this.itemCacheableServiceImpl.findItemCodesByPristine(STORE_ID, pristineDataItem);
    Mockito.verify(cacheItemHelperService).findItemCodesByPristine(STORE_ID, pristineDataItem);
  }

  @Test
  public void findCacheableItemCodesByStoreIdAndPristineAndMarkForDeleteFalseTest() {
    Mockito.when(
        cacheItemHelperService.findCacheableItemCodesByStoreIdAndPristineAndMarkForDeleteFalse(STORE_ID, PRISTINE_ID))
        .thenReturn(new HashSet<>());
    Set<String> result = this.itemCacheableServiceImpl
        .findCacheableItemCodesByStoreIdAndPristineAndMarkForDeleteFalse(STORE_ID, PRISTINE_ID);
    Mockito.verify(cacheItemHelperService)
        .findCacheableItemCodesByStoreIdAndPristineAndMarkForDeleteFalse(STORE_ID, PRISTINE_ID);
  }

  @Test
  public void getItemsByPristineIdTest() {
    Mockito.when(
            cacheItemHelperService.findCacheableItemSkusByStoreIdAndPristineIdAndMarkForDeleteFalse(STORE_ID, PRISTINE_ID))
        .thenReturn(ImmutableSet.of(ITEM_SKU));
    Mockito.when(cacheItemHelperService.getCacheableItemsByItemSkus(STORE_ID, ImmutableSet.of(ITEM_SKU))).thenReturn(
        ImmutableMap.of(ITEM_SKU, item1));
    List<Item> result = this.itemCacheableServiceImpl.getItemsByPristineId(STORE_ID, PRISTINE_ID, false);
    Mockito.verify(cacheItemHelperService)
        .findCacheableItemSkusByStoreIdAndPristineIdAndMarkForDeleteFalse(STORE_ID, PRISTINE_ID);
    Mockito.verify(cacheItemHelperService).getCacheableItemsByItemSkus(STORE_ID, ImmutableSet.of(ITEM_SKU));
  }

  @Test
  public void getItemsByPristineIdStoreIdBlankTest() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.itemCacheableServiceImpl.getItemsByPristineId(StringUtils.EMPTY, PRISTINE_ID, false));
  }

  @Test
  public void getItemsByPristineIdPristineIdlankTest() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.itemCacheableServiceImpl.getItemsByPristineId(STORE_ID, StringUtils.EMPTY, false));
  }

  @Test
  public void findItemByStoreIdAndItemSkuAndMarkForDeleteFalseFbbActiveTrue() {
    Mockito.when(cacheItemHelperService
      .findCacheableByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, ITEM_SKU))
      .thenReturn(this.item1);
    Mockito.when(itemPickupPointService
      .findByStoreIdAndItemSkuAndFbbActivatedTrueAndMarkForDeleteFalse(Mockito.anyString(),
        Mockito.anyString())).thenReturn(itemPickupPoint);
    Item result = this.itemCacheableServiceImpl
      .findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, ITEM_SKU, true, true, false, null,
        false, true);
    Mockito.verify(cacheItemHelperService)
      .findCacheableByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, ITEM_SKU);
    Mockito.verify(itemPickupPointService)
      .findByStoreIdAndItemSkuAndFbbActivatedTrueAndMarkForDeleteFalse(Mockito.anyString(),
        Mockito.anyString());
    Mockito.verify(itemPickupPointService)
      .findByStoreIdAndItemSkuInAndMarkForDeleteFalseAndFbbActivated(Mockito.anyString(),
        Mockito.anySet(),Mockito.anyBoolean());

    Mockito.verify(cacheItemHelperService)
      .findCacheableActivePromoBundlingsByStoreIdAndPristine(STORE_ID, this.pristineDataItem);
    Mockito.verify(objectConverterService).overrideL4DetailsFromL5(Mockito.anyList(), Mockito.anyList());
  }

  @Test
  public void findItemAndItemPickPointByproductSkusAndCncActive_Test() {
    Item item = new Item();
    item.setItemSku(ITEM_SKU);
    Set<String> itemSkus = new HashSet<>();
    itemSkus.add(ITEM_SKU);
    this.activePromoBundlings.add("WHOLESALE");
    this.activePromoBundlings.add("COMBO");
    itemPickupPointPage =
        new PageImpl<>(Arrays.asList(itemPickupPoint), PageRequest.of(PAGE, PAGE_SIZE),
            TOTAL_L5_COUNT);
    Mockito.when(
        itemPickupPointService.findItemPickupPointByProductSkus(STORE_ID, Arrays.asList(PRODUCTSKU),
            false, PAGE, PAGE_SIZE)).thenReturn(itemPickupPointPage);
    Mockito.when(
            itemService.findItemsByStoreIdAndItemSkuInAndMarkForDeleteFalse(STORE_ID, itemSkus))
        .thenReturn(Arrays.asList(item));
    ItemAndItemPickupPointVo result =
        this.itemCacheableServiceImpl.findItemAndItemPickPointByproductSkusAndCncActive(STORE_ID,
            Arrays.asList(PRODUCTSKU), null, false, PAGE, PAGE_SIZE);

    Mockito.verify(itemPickupPointService)
        .findItemPickupPointByProductSkus(STORE_ID, Arrays.asList(PRODUCTSKU), false, PAGE,
            PAGE_SIZE);
    Mockito.verify(itemService)
        .findItemsByStoreIdAndItemSkuInAndMarkForDeleteFalse(STORE_ID, itemSkus);
    Assertions.assertNotNull(result);
  }

  @Test
  public void findItemAndItemPickPointByOfflineItemIds() {
    Item item = new Item();
    item.setItemSku(ITEM_SKU);
    Set<String> itemSkus = new HashSet<>();
    itemSkus.add(ITEM_SKU);
    this.activePromoBundlings.add("WHOLESALE");
    this.activePromoBundlings.add("COMBO");
    itemPickupPointPage =
      new PageImpl<>(Arrays.asList(itemPickupPoint), PageRequest.of(PAGE, PAGE_SIZE),
        TOTAL_L5_COUNT);
    Mockito.when(
      itemPickupPointService.findByStoreIdAndOfflineItemIds(STORE_ID,
        Collections.singletonList(L5_ID))).thenReturn(itemPickupPointPage.getContent());
    Mockito.when(
        itemService.findItemsByStoreIdAndItemSkuInAndMarkForDeleteFalse(STORE_ID, itemSkus))
      .thenReturn(Arrays.asList(item));
    ItemAndItemPickupPointVo result =
      this.itemCacheableServiceImpl.findItemAndItemPickPointByproductSkusAndCncActive(STORE_ID,
        null, Collections.singletonList(L5_ID), false, PAGE, PAGE_SIZE);

    Mockito.verify(itemPickupPointService)
      .findByStoreIdAndOfflineItemIds(STORE_ID, Collections.singletonList(L5_ID));
    Mockito.verify(itemService)
      .findItemsByStoreIdAndItemSkuInAndMarkForDeleteFalse(STORE_ID, itemSkus);
    Assertions.assertNotNull(result);
  }

  @Test
  public void findItemAndItemPickPointByproductSkusAndCncActiveEmptyListTest() {
    Item item = new Item();
    item.setItemSku(ITEM_SKU);
    Set<String> itemSkus = new HashSet<>();
    itemSkus.add(ITEM_SKU);
    this.activePromoBundlings.add("WHOLESALE");
    this.activePromoBundlings.add("COMBO");
    itemPickupPointPage =
        new PageImpl<>(Arrays.asList(), PageRequest.of(PAGE, PAGE_SIZE), TOTAL_L5_COUNT);
    Mockito.when(
        itemPickupPointService.findItemPickupPointByProductSkus(STORE_ID, Arrays.asList(PRODUCTSKU), false, PAGE,
            PAGE_SIZE)).thenReturn(itemPickupPointPage);
    Mockito.when(itemService.findItemsByStoreIdAndItemSkuInAndMarkForDeleteFalse(STORE_ID, itemSkus))
        .thenReturn(Arrays.asList(item));
    ItemAndItemPickupPointVo result =
        this.itemCacheableServiceImpl.findItemAndItemPickPointByproductSkusAndCncActive(STORE_ID,
            Arrays.asList(PRODUCTSKU), null, false, PAGE, PAGE_SIZE);
    Mockito.verify(itemPickupPointService)
        .findItemPickupPointByProductSkus(STORE_ID, Arrays.asList(PRODUCTSKU), false, PAGE, PAGE_SIZE);
    Assertions.assertNull(result);
  }

  @Test
  public void findItemsByStoreIdAndProductSkuFetchItemPickupPointWithoutDeliveryTrueTest() {
    ReflectionTestUtils.setField(itemCacheableServiceImpl, "fetchItemPickupPointWithoutDelivery", true);
    List<Item> items = new ArrayList<>();
    this.item2.setOff2OnChannelActive(true);
    items.add(item1);
    items.add(item2);
    Mockito.when(cacheItemHelperService.findCacheableByStoreIdAndProductSku(STORE_ID, PRODUCTSKU)).thenReturn(items);
    List<Item> result =
        itemCacheableServiceImpl.findItemsByStoreIdAndProductSku(STORE_ID, PRODUCTSKU, false, true, false);
    Mockito.verify(cacheItemHelperService).findCacheableByStoreIdAndProductSku(STORE_ID, PRODUCTSKU);
    Mockito.verify(itemPickupPointService).findByItemSkusAndDelivery(STORE_ID, Arrays.asList(ITEM_SKU2), true);
    Assertions.assertEquals(1, result.size());
    Assertions.assertTrue(result.get(0).isOff2OnChannelActive());
  }
  @Test
  public void findItemsByStoreIdAndProductSkuFetchItemPickupPointWithoutDeliveryTrueIsMigratedTest() {
    ReflectionTestUtils.setField(itemCacheableServiceImpl, "fetchItemPickupPointWithoutDelivery", true);
    List<Item> items = new ArrayList<>();
    this.item2.setOff2OnChannelActive(true);
    items.add(item1);
    items.add(item2);
    Mockito.when(cacheItemHelperService.findCacheableByStoreIdAndProductSku(STORE_ID, PRODUCTSKU)).thenReturn(items);
    List<Item> result =
        itemCacheableServiceImpl.findItemsByStoreIdAndProductSku(STORE_ID, PRODUCTSKU, false, true, true);
    Mockito.verify(cacheItemHelperService).findCacheableByStoreIdAndProductSku(STORE_ID, PRODUCTSKU);
    Mockito.verify(itemPickupPointService).findOneForEachItemSkuIn(STORE_ID, Arrays.asList(ITEM_SKU2));
    Assertions.assertEquals(1, result.size());
    Assertions.assertTrue(result.get(0).isOff2OnChannelActive());
  }
}
