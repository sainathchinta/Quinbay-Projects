package com.gdn.x.product.service.impl;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.anyBoolean;
import static org.mockito.ArgumentMatchers.anyList;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;
import static org.mockito.MockitoAnnotations.openMocks;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import com.gdn.x.product.model.entity.Product;
import com.gdn.x.product.service.api.ProductService;
import org.apache.commons.beanutils.BeanUtils;
import org.apache.commons.lang3.StringUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.springframework.test.util.ReflectionTestUtils;

import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.x.product.domain.event.model.ItemPickupPointDataChangeEventModel;
import com.gdn.x.product.enums.Constants;
import com.gdn.x.product.enums.ItemChangeEventType;
import com.gdn.x.product.enums.ProductFieldNames;
import com.gdn.x.product.model.entity.Item;
import com.gdn.x.product.model.entity.ItemBuyableSchedule;
import com.gdn.x.product.model.entity.ItemDiscoverableSchedule;
import com.gdn.x.product.model.entity.ItemPickupPoint;
import com.gdn.x.product.model.entity.ItemViewConfig;
import com.gdn.x.product.model.entity.Price;
import com.gdn.x.product.model.vo.ItemVo;
import com.gdn.x.product.rest.web.model.dto.AuditTrailDto;
import com.gdn.x.product.rest.web.model.dto.ItemBuyableScheduleDTO;
import com.gdn.x.product.rest.web.model.dto.ItemDiscoverableScheduleDTO;
import com.gdn.x.product.rest.web.model.request.ItemViewConfigAndItemSkuRequest;
import com.gdn.x.product.service.api.CacheEvictHelperService;
import com.gdn.x.product.service.api.ChannelService;
import com.gdn.x.product.service.api.ItemHelperService;
import com.gdn.x.product.service.api.ItemPickupPointService;
import com.gdn.x.product.service.api.ItemPickupPointSummaryService;
import com.gdn.x.product.service.api.ItemService;
import com.gdn.x.product.service.api.ObjectConverterService;
import com.gdn.x.product.service.api.ProductAndItemSolrIndexerService;
import com.gdn.x.product.service.api.ProductHelperService;
import com.gdn.x.product.service.api.SaveAndPublishService;
import com.gdn.x.product.service.api.SaveOperationService;

/**
 * This unit test class template is the work of maria.o.a.sunaryo and felix.w.wijaya as
 * participation in Blibli Hackathon #1. Generated on 24 Jun 2016 22:46:25
 */
public class ItemViewConfigServiceImplTest {

  private static final String ITEM_SKU_NOT_FOUND = "item-sku-not-found";
  private static final ItemViewConfig ITEM_VIEW_CONFIG = new ItemViewConfig();
  private static Item ITEM;
  private static final String STORE_ID = "store-id";
  private static final String CHANNEL = Constants.DEFAULT;
  private static final String ITEM_SKU = "item-sku";
  private static final String ITEM_SKU_2 = "item-sku-2";
  private static final String MERCHANT_SKU = "merchantSku";
  private static final String MERCHANT_CODE = "merchantCode";
  private static final String PRODUCT_SKU  = "product-sku";
  private static final String PICKUP_POINT_CODE = "pickupPointCode";

  private Map<String, ItemViewConfig> itemViewConfigMap;
  private ItemViewConfigAndItemSkuRequest itemViewConfigAndItemSkuRequest;
  Set<ItemViewConfig> itemViewConfigSet = new HashSet<>();
  ItemViewConfig itemViewConfig = new ItemViewConfig();

  @InjectMocks
  private ItemViewConfigServiceImpl itemViewConfigServiceImpl;
  @Mock
  private ItemService itemService;
  @Mock
  private ItemHelperService itemHelperService;
  @Mock
  private SaveOperationService saveOperationService;

  @Mock
  private ChannelService channelService;

  @Mock
  private SaveAndPublishService saveAndPublishService;

  @Mock
  private ProductHelperService productHelperService;

  @Mock
  private ProductAndItemSolrIndexerService productAndItemSolrIndexerService;

  @Mock
  private CacheEvictHelperService cacheEvictHelperService;

  @Mock
  private ItemPickupPointService itemPickupPointService;

  @Mock
  private ObjectConverterService objectConverterService;

  @Mock
  private ItemPickupPointSummaryService itemPickupPointSummaryService;

  @Mock
  private ProductService productService;

  @Captor
  private ArgumentCaptor<List<AuditTrailDto>> auditTrailDtoListArgumentCaptor;

  private ItemPickupPoint itemPickupPoint;
  private Set<Price> prices1;
  private Product product;

  @BeforeEach
  public void setUp() throws Exception {
    openMocks(this);
    itemViewConfig.setChannel(CHANNEL);
    itemViewConfig.setBuyable(true);
    itemViewConfigSet.add(itemViewConfig);
    ITEM = new Item();
    ITEM.setItemViewConfigs(itemViewConfigSet);
    when(
        this.itemService.findByStoreIdAndItemSkuAndMarkForDeleteFalse(
            ItemViewConfigServiceImplTest.STORE_ID,
            ItemViewConfigServiceImplTest.ITEM_SKU_NOT_FOUND)).thenReturn(null);
    when(
        this.itemService.findByStoreIdAndItemSkuAndMarkForDeleteFalse(
            ItemViewConfigServiceImplTest.STORE_ID, ItemViewConfigServiceImplTest.ITEM_SKU))
        .thenReturn(ItemViewConfigServiceImplTest.ITEM);
    itemPickupPoint =
        ItemPickupPoint.builder().itemSku(ITEM_SKU).price(prices1).itemViewConfig(itemViewConfigSet).build();
    itemPickupPoint.setItemViewConfig(itemViewConfigSet);
    itemPickupPoint.setPickupPointCode(PICKUP_POINT_CODE);
    when(this.itemPickupPointService.findByItemSkuAndDelivery(STORE_ID, ITEM_SKU))
        .thenReturn(itemPickupPoint);
    itemViewConfigMap = new HashMap<>();
    itemViewConfigMap.put(ITEM_SKU, itemViewConfig);
    prices1 = new HashSet<>(Arrays.asList(new Price("$", 10, 10, "", "", new Date())));
    Mockito.when(itemPickupPointService.findByItemSkuAndDelivery(Mockito.anyString(), Mockito.anyString()))
        .thenReturn(itemPickupPoint);

    itemViewConfigAndItemSkuRequest = new ItemViewConfigAndItemSkuRequest();
    itemViewConfigAndItemSkuRequest.setItemSku(ITEM_SKU);
    itemViewConfigAndItemSkuRequest.setPickupPointCode(PICKUP_POINT_CODE);
    ReflectionTestUtils.setField(itemViewConfigServiceImpl, "takeDownSwitch", true);
    ReflectionTestUtils.setField(itemViewConfigServiceImpl, "setArchivedBeforeEditFlag", true);
    product = new Product();
    product.setStoreId(STORE_ID);
    product.setProductSku(PRODUCT_SKU);
  }

  @AfterEach
  public void tearDown() throws Exception {
    verifyNoMoreInteractions(this.itemService);
    verifyNoMoreInteractions(this.itemHelperService);
    verifyNoMoreInteractions(this.saveOperationService);
    verifyNoMoreInteractions(this.saveAndPublishService);
    verifyNoMoreInteractions(this.productHelperService);
    verifyNoMoreInteractions(objectConverterService);
  }

  @Test
  public void updateBuyableScheduleMultiPickupPointEnabledTest() {
    ItemBuyableSchedule itemBuyableSchedule =
        new ItemBuyableSchedule(false, new Date(), new Date());
    boolean result =
        this.itemViewConfigServiceImpl.updateBuyableSchedule(
            ItemViewConfigServiceImplTest.STORE_ID, ItemViewConfigServiceImplTest.CHANNEL,
            ItemViewConfigServiceImplTest.ITEM_SKU, itemBuyableSchedule);
    assertTrue(result);
    verify(this.itemService).findByStoreIdAndItemSkuAndMarkForDeleteFalse(
        ItemViewConfigServiceImplTest.STORE_ID, ItemViewConfigServiceImplTest.ITEM_SKU);
    verify(this.saveOperationService).updateItemPickupPointFieldsByItemSku(
        ItemViewConfigServiceImplTest.STORE_ID, ItemViewConfigServiceImplTest.ITEM_SKU,
        ProductFieldNames.ITEM_PICKUP_POINT_DISPLAYABLE_SCHEDULES,
        itemBuyableSchedule, ITEM);
  }

  @Test
  public void updateBuyableScheduleWithItemNotFoundTest() {
    ItemBuyableSchedule itemBuyableSchedule =
        new ItemBuyableSchedule(false, new Date(), new Date());
    try {
      this.itemViewConfigServiceImpl.updateBuyableSchedule(ItemViewConfigServiceImplTest.STORE_ID,
          ItemViewConfigServiceImplTest.CHANNEL, ItemViewConfigServiceImplTest.ITEM_SKU_NOT_FOUND,
          itemBuyableSchedule);
    } catch (ApplicationRuntimeException e) {
      verify(this.itemService).findByStoreIdAndItemSkuAndMarkForDeleteFalse(
          ItemViewConfigServiceImplTest.STORE_ID, ItemViewConfigServiceImplTest.ITEM_SKU_NOT_FOUND);
    }
  }

  @Test
  public void updateDefaultBuyableTest() {
    boolean defaultBuyable = true;
    ItemViewConfig itemViewConfig = new ItemViewConfig();
    when(
        this.itemHelperService.getItemViewConfigs(
            ItemViewConfigServiceImplTest.ITEM.getItemViewConfigs(),
            ItemViewConfigServiceImplTest.CHANNEL)).thenReturn(itemViewConfig);
    boolean expectedResult = true;
    boolean result =
        this.itemViewConfigServiceImpl.updateDefaultBuyable(ItemViewConfigServiceImplTest.STORE_ID,
            ItemViewConfigServiceImplTest.CHANNEL, ItemViewConfigServiceImplTest.ITEM_SKU,
            defaultBuyable);
    assertEquals(itemViewConfig.isBuyable(), defaultBuyable);
    assertEquals(result, expectedResult);
    verify(this.itemService).findByStoreIdAndItemSkuAndMarkForDeleteFalse(
        ItemViewConfigServiceImplTest.STORE_ID, ItemViewConfigServiceImplTest.ITEM_SKU);
    verify(this.itemHelperService).getItemViewConfigs(
        ItemViewConfigServiceImplTest.ITEM.getItemViewConfigs(),
        ItemViewConfigServiceImplTest.CHANNEL);
    verify(this.saveOperationService).updateItemFieldByItemSkuAndPickupPoint(
        ItemViewConfigServiceImplTest.STORE_ID, ItemViewConfigServiceImplTest.ITEM_SKU,
        ProductFieldNames.ITEM_VIEW_CONFIGS,
        ItemViewConfigServiceImplTest.ITEM.getItemViewConfigs(), ITEM);
    verify(productAndItemSolrIndexerService).updateSolrOnItemViewConfigChanges(Arrays.asList(ITEM));
  }

  @Test
  public void updateDefaultBuyableWithItemNotFoundTest() {
    boolean defaultBuyable = true;
    try {
      this.itemViewConfigServiceImpl.updateDefaultBuyable(ItemViewConfigServiceImplTest.STORE_ID,
          ItemViewConfigServiceImplTest.CHANNEL, ItemViewConfigServiceImplTest.ITEM_SKU_NOT_FOUND,
          defaultBuyable);
    } catch (ApplicationRuntimeException e) {
      verify(this.itemService).findByStoreIdAndItemSkuAndMarkForDeleteFalse(
          ItemViewConfigServiceImplTest.STORE_ID, ItemViewConfigServiceImplTest.ITEM_SKU_NOT_FOUND);
    }
  }


  @Test
  public void updateDefaultDiscoverableTest() {
    boolean defaultDiscoverable = true;
    ItemViewConfig itemViewConfig = new ItemViewConfig();
    when(
        this.itemHelperService.getItemViewConfigs(
            ItemViewConfigServiceImplTest.ITEM.getItemViewConfigs(),
            ItemViewConfigServiceImplTest.CHANNEL)).thenReturn(itemViewConfig);
    boolean expectedResult = true;
    boolean result =
        this.itemViewConfigServiceImpl.updateDefaultDiscoverable(
            ItemViewConfigServiceImplTest.STORE_ID, ItemViewConfigServiceImplTest.CHANNEL,
            ItemViewConfigServiceImplTest.ITEM_SKU, defaultDiscoverable);
    assertEquals(itemViewConfig.isDiscoverable(), defaultDiscoverable);
    assertEquals(result, expectedResult);
    verify(this.itemService).findByStoreIdAndItemSkuAndMarkForDeleteFalse(
        ItemViewConfigServiceImplTest.STORE_ID, ItemViewConfigServiceImplTest.ITEM_SKU);
    verify(this.itemHelperService).getItemViewConfigs(
        ItemViewConfigServiceImplTest.ITEM.getItemViewConfigs(),
        ItemViewConfigServiceImplTest.CHANNEL);
    verify(this.saveOperationService).updateItemFieldByItemSkuAndPickupPoint(
        ItemViewConfigServiceImplTest.STORE_ID, ItemViewConfigServiceImplTest.ITEM_SKU,
        ProductFieldNames.ITEM_VIEW_CONFIGS,
        ItemViewConfigServiceImplTest.ITEM.getItemViewConfigs(), ITEM);
    verify(saveAndPublishService).publishMerchantVoucherViewConfigChange(anyList(), anyList());
    verify(productAndItemSolrIndexerService).updateSolrOnItemViewConfigChanges(Arrays.asList(ITEM));
  }

  @Test
  public void updateDefaultDiscoverableWithItemNotFoundTest() {
    boolean defaultDiscoverable = true;
    try {
      this.itemViewConfigServiceImpl.updateDefaultDiscoverable(
          ItemViewConfigServiceImplTest.STORE_ID, ItemViewConfigServiceImplTest.CHANNEL,
          ItemViewConfigServiceImplTest.ITEM_SKU_NOT_FOUND, defaultDiscoverable);
    } catch (ApplicationRuntimeException e) {
      verify(this.itemService).findByStoreIdAndItemSkuAndMarkForDeleteFalse(
          ItemViewConfigServiceImplTest.STORE_ID, ItemViewConfigServiceImplTest.ITEM_SKU_NOT_FOUND);
    }
  }

  @Test
  public void updateDiscoverableScheduleMultiPickupPointEnabledTest() {
    ItemDiscoverableSchedule itemDiscoverableSchedule =
        new ItemDiscoverableSchedule(false, new Date(), new Date());
    boolean result =
        this.itemViewConfigServiceImpl.updateDiscoverableSchedule(
            ItemViewConfigServiceImplTest.STORE_ID, ItemViewConfigServiceImplTest.CHANNEL,
            ItemViewConfigServiceImplTest.ITEM_SKU, itemDiscoverableSchedule);
    assertTrue(result);
    verify(this.itemService).findByStoreIdAndItemSkuAndMarkForDeleteFalse(
        ItemViewConfigServiceImplTest.STORE_ID, ItemViewConfigServiceImplTest.ITEM_SKU);
    verify(this.saveOperationService).updateItemPickupPointFieldsByItemSku(
        ItemViewConfigServiceImplTest.STORE_ID, ItemViewConfigServiceImplTest.ITEM_SKU,
        ProductFieldNames.ITEM_PICKUP_POINT_DISCOVERABLE_SCHEDULES,
        itemDiscoverableSchedule, ITEM);
  }

  @Test
  public void updateDiscoverableScheduleWithItemNotFoundTest() {
    ItemDiscoverableSchedule itemBuyableSchedule =
        new ItemDiscoverableSchedule(false, new Date(), new Date());
    try {
      this.itemViewConfigServiceImpl.updateDiscoverableSchedule(
          ItemViewConfigServiceImplTest.STORE_ID, ItemViewConfigServiceImplTest.CHANNEL,
          ItemViewConfigServiceImplTest.ITEM_SKU_NOT_FOUND, itemBuyableSchedule);
    } catch (ApplicationRuntimeException e) {
      verify(this.itemService).findByStoreIdAndItemSkuAndMarkForDeleteFalse(
          ItemViewConfigServiceImplTest.STORE_ID, ItemViewConfigServiceImplTest.ITEM_SKU_NOT_FOUND);
    }
  }

  @Test
  public void isItemViewConfigChangeForExistingChannelChangeTest(){
    Set<ItemViewConfig> newItemViewConfigSet = new HashSet<>();
    ItemViewConfig newItemViewConfig = new ItemViewConfig();
    newItemViewConfigSet.add(newItemViewConfig);

    Mockito.when(this.channelService.getDefaultChannel()).thenReturn(CHANNEL);
    boolean result = this.itemViewConfigServiceImpl
        .isItemViewConfigChangeForExistingChannelChange(ITEM, newItemViewConfigSet);
    Mockito.verify(this.channelService).getDefaultChannel();
    Assertions.assertTrue(result);
  }

  @Test
  public void isItemViewConfigChangeForExistingChannelChange_whenDiscoverableFlagChangeTest(){
    Set<ItemViewConfig> newItemViewConfigSet = new HashSet<>();
    ItemViewConfig newItemViewConfig = new ItemViewConfig();
    newItemViewConfig.setBuyable(true);
    newItemViewConfig.setDiscoverable(true);
    newItemViewConfigSet.add(newItemViewConfig);

    Mockito.when(this.channelService.getDefaultChannel()).thenReturn(CHANNEL);
    boolean result = this.itemViewConfigServiceImpl
        .isItemViewConfigChangeForExistingChannelChange(ITEM, newItemViewConfigSet);
    Mockito.verify(this.channelService).getDefaultChannel();
    Assertions.assertTrue(result);
  }

  @Test
  public void isItemViewConfigChangeForExistingChannelChange_whenNoChangeTest(){

    Date date = new Date();
    Set<ItemViewConfig> newItemViewConfigSet = new HashSet<>();
    ItemViewConfig newItemViewConfig = new ItemViewConfig();
    newItemViewConfig.setBuyable(true);
    newItemViewConfigSet.add(newItemViewConfig);
    ItemDiscoverableSchedule itemDiscoverableSchedule =
        new ItemDiscoverableSchedule(false, date, date);
    newItemViewConfig.setItemDiscoverableScheduleIfNotNull(itemDiscoverableSchedule);
    ITEM.getItemViewConfigs().iterator().next()
        .setItemDiscoverableScheduleIfNotNull(itemDiscoverableSchedule);
    Mockito.when(this.channelService.getDefaultChannel()).thenReturn(CHANNEL);
    boolean result = this.itemViewConfigServiceImpl
        .isItemViewConfigChangeForExistingChannelChange(ITEM, newItemViewConfigSet);
    Mockito.verify(this.channelService).getDefaultChannel();
    Assertions.assertFalse(result);
  }

  @Test
  public void isItemViewConfigChangeForExistingChannelChange_whenhangeWhenScheduleCTest(){

    Date date = new Date();
    Set<ItemViewConfig> newItemViewConfigSet = new HashSet<>();
    ItemViewConfig newItemViewConfig = new ItemViewConfig();
    newItemViewConfig.setBuyable(true);
    newItemViewConfigSet.add(newItemViewConfig);
    ItemDiscoverableSchedule newItemDiscoverableSchedule =
        new ItemDiscoverableSchedule(false, date, date);
    newItemViewConfig.setItemDiscoverableScheduleIfNotNull(newItemDiscoverableSchedule);
    ItemDiscoverableSchedule itemDiscoverableSchedule =
        new ItemDiscoverableSchedule(true, date, date);
    ITEM.getItemViewConfigs().iterator().next()
        .setItemDiscoverableScheduleIfNotNull(itemDiscoverableSchedule);
    Mockito.when(this.channelService.getDefaultChannel()).thenReturn(CHANNEL);
    boolean result = this.itemViewConfigServiceImpl
        .isItemViewConfigChangeForExistingChannelChange(ITEM, newItemViewConfigSet);
    Mockito.verify(this.channelService).getDefaultChannel();
    Assertions.assertTrue(result);
  }

  @Test
  public void updateItemViewConfigTest() throws Exception {
    when(productHelperService.updateItemViewConfigForExistingChannelItemPickupPoint(itemPickupPoint,
        ITEM_VIEW_CONFIG)).thenReturn(true);
    when(itemPickupPointService.findByItemSkuAndDelivery(STORE_ID,
        ITEM_SKU)).thenReturn(itemPickupPoint);
    when(itemPickupPointService.saveItemPickupPoint(Mockito.any(ItemPickupPoint.class))).thenReturn(new ItemPickupPoint());
    doNothing().when(cacheEvictHelperService)
        .evictItemPickupPointData(Mockito.anyString(), Mockito.any(ItemPickupPoint.class), Mockito.anyString());
    itemViewConfigServiceImpl.updateItemViewConfig(STORE_ID, ITEM_SKU, ITEM_VIEW_CONFIG);
    verify(itemService).findByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, ITEM_SKU);
    verify(productHelperService).updateItemViewConfigForExistingChannelItemPickupPoint(itemPickupPoint,
        ITEM_VIEW_CONFIG);
    verify(productAndItemSolrIndexerService).updateSolrOnItemViewConfigChangesByItemPickupPoint(
            anyList());
    verify(saveAndPublishService).publishMerchantVoucherViewConfigChange(Mockito.any(ItemVo.class));
    verify(itemPickupPointService).findByItemSkuAndDelivery(STORE_ID, ITEM_SKU);
    verify(itemPickupPointService).saveItemPickupPoint(Mockito.any(ItemPickupPoint.class));
    verify(cacheEvictHelperService).evictItemPickupPointData(Mockito.isNull(), Mockito.any(ItemPickupPoint.class),
        Mockito.isNull());
    verify(cacheEvictHelperService).evictItemPickupPointData(Mockito.isNull(), Mockito.any(ItemPickupPoint.class),
        Mockito.isNull());
    verify(cacheEvictHelperService).evictItemData(Mockito.isNull(), Mockito.any(Item.class));
    verify(saveAndPublishService).publishItemPickupPointDataChangeEvent(Mockito.any(),Mockito.any(),
      eq(Collections.EMPTY_MAP));
  }

  @Test
  public void updateItemViewConfigNullItemPickupPointTest() throws Exception {
    when(itemPickupPointService.findByItemSkuAndDelivery(STORE_ID,
        ITEM_SKU)).thenReturn(null);
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> itemViewConfigServiceImpl.updateItemViewConfig(STORE_ID, ITEM_SKU, ITEM_VIEW_CONFIG));
    } finally {
      verify(itemService).findByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, ITEM_SKU);
    }
  }

  @Test
  public void updateItemViewConfigByMerchantSkuAndMerchantCodeTest() {
    Item item = new Item();
    when(this.itemService.getItemsByMerchantSkuAndMerchantCode(STORE_ID, MERCHANT_SKU, MERCHANT_CODE))
        .thenReturn(Arrays.asList(item));
    itemViewConfigServiceImpl.updateItemViewConfigByMerchantSkuAndMerchantCode(STORE_ID, "",
        "", MERCHANT_SKU, MERCHANT_CODE, ITEM_VIEW_CONFIG);
    verify(saveOperationService).saveItemsAndClearCacheWithoutUpdatingSolr(Arrays.asList(item), null, StringUtils.EMPTY);
    verify(productAndItemSolrIndexerService).updateSolrOnItemViewConfigChanges(Arrays.asList(item));
    verify(saveAndPublishService).publishMerchantVoucherViewConfigChange(anyList(), anyList());
    verify(this.itemService).getItemsByMerchantSkuAndMerchantCode(STORE_ID, MERCHANT_SKU, MERCHANT_CODE);
  }

  @Test
  public void updateItemViewConfigAndForceReviewTest() {
    ReflectionTestUtils.setField(itemViewConfigServiceImpl, "cncForWarehouseFeatureSwitch", true);
    ITEM.setProductSku(PRODUCT_SKU);
    ITEM.setItemSku(ITEM_SKU);
    itemPickupPoint.setItemViewConfig(itemViewConfigSet);
    itemPickupPoint.getAllItemViewConfigs().add(ItemViewConfig.builder().channel(Constants.CNC).isDiscoverable(false).build());
    itemViewConfigAndItemSkuRequest.setItemBuyableSchedules(new ItemBuyableScheduleDTO());
    itemViewConfigAndItemSkuRequest.setChannel(null);
    itemViewConfigAndItemSkuRequest.setDiscoverable(true);
    Mockito.when(this.itemService.findByStoreIdAndItemSku(STORE_ID, ITEM_SKU)).thenReturn(ITEM);
    Mockito.when(this.saveOperationService.saveItemsAndClearCacheWithoutUpdatingSolr(Arrays.asList(ITEM), null, StringUtils.EMPTY))
        .thenReturn(Arrays.asList(ITEM));
    Mockito.doNothing().when(this.productAndItemSolrIndexerService).updateSolrOnItemViewConfigChanges(Collections.singletonList(ITEM));
    Mockito.doNothing().when(saveAndPublishService).publishMerchantVoucherViewConfigChange(anyList(), anyList());
    Mockito.when(itemPickupPointService.findByItemSkuAndPickupPointCode(STORE_ID, ITEM_SKU, PICKUP_POINT_CODE)).thenReturn(itemPickupPoint);
    Mockito.when(channelService.getDefaultChannel()).thenReturn(CHANNEL);
    Mockito.when(objectConverterService.convertToItemPickupPointChangeEventModel(Mockito.any(ItemPickupPoint.class),
        Mockito.anyBoolean())).thenReturn(new ItemPickupPointDataChangeEventModel());
    itemViewConfigServiceImpl.updateItemViewConfigAndForceReview(STORE_ID, Arrays.asList(itemViewConfigAndItemSkuRequest), true,
        false, PRODUCT_SKU, true);
    Mockito.verify(this.itemService).findByStoreIdAndItemSku(STORE_ID, ITEM_SKU);
    Mockito.verify(this.saveOperationService).saveItemsAndClearCacheWithoutUpdatingSolrWithoutPublishing(Arrays.asList(ITEM));
    Mockito.verify(saveAndPublishService)
        .publishListOfItems(Mockito.eq(Arrays.asList(ITEM)), anyList(), anyList(), anyBoolean());
    Mockito.verify(this.productAndItemSolrIndexerService).updateSolrOnItemViewConfigChanges(Arrays.asList(ITEM));
    Mockito.verify(saveAndPublishService).publishMerchantVoucherViewConfigChange(anyList(), anyList());
    Mockito.verify(itemPickupPointService).saveItemPickupPoint(Arrays.asList(itemPickupPoint));
    Mockito.verify(objectConverterService)
        .convertToItemPickupPointChangeEventModel(Mockito.any(ItemPickupPoint.class), Mockito.anyBoolean());
    Mockito.verify(objectConverterService).overrideL4DetailsFromL5(Arrays.asList(ITEM), new ArrayList<>());
    Assertions.assertEquals(ItemChangeEventType.ARCHIVED_FLAG_CHANGE, ITEM.getItemChangeEventTypes().get(0));
    Mockito.verify(itemPickupPointSummaryService).updateExternalHistoryInPBP(auditTrailDtoListArgumentCaptor.capture());
    Assertions.assertTrue(ITEM.isArchived());
    Assertions.assertEquals(1, auditTrailDtoListArgumentCaptor.getValue().size());
  }

  @Test
  public void updateItemViewConfigAndForceReviewTest_Cnc1P()
      throws Exception {
    ReflectionTestUtils.setField(itemViewConfigServiceImpl, "cncForWarehouseFeatureSwitch", true);
    ITEM.setProductSku(PRODUCT_SKU);
    ITEM.setItemSku(ITEM_SKU);
    itemPickupPoint.setItemViewConfig(itemViewConfigSet);
    itemViewConfigAndItemSkuRequest.setItemBuyableSchedules(new ItemBuyableScheduleDTO());
    itemViewConfigAndItemSkuRequest.setChannel(null);
    itemViewConfigAndItemSkuRequest.setDiscoverable(true);
    Mockito.when(this.itemService.findByStoreIdAndItemSku(STORE_ID, ITEM_SKU)).thenReturn(ITEM);
    Mockito.when(this.saveOperationService.saveItemsAndClearCacheWithoutUpdatingSolr(Arrays.asList(ITEM), null, StringUtils.EMPTY))
        .thenReturn(Arrays.asList(ITEM));
    Mockito.when(this.productService.findByStoreIdAndProductSkuIn(STORE_ID, List.of(PRODUCT_SKU))).thenReturn(List.of(product));
    Mockito.doNothing().when(this.productAndItemSolrIndexerService).updateSolrOnItemViewConfigChanges(anyList());
    Mockito.doNothing().when(saveAndPublishService).publishMerchantVoucherViewConfigChange(anyList(), anyList());
    Mockito.when(itemPickupPointService.findByItemSkuAndPickupPointCode(STORE_ID, ITEM_SKU, PICKUP_POINT_CODE)).thenReturn(itemPickupPoint);
    Mockito.when(channelService.getDefaultChannel()).thenReturn(CHANNEL);
    Mockito.when(itemService.existsRecordForStoreIdAndProductSkuAndCncActivated(STORE_ID, PRODUCT_SKU, Boolean.TRUE)).thenReturn(Boolean.TRUE);
    Mockito.when(objectConverterService.convertToItemPickupPointChangeEventModel(Mockito.any(ItemPickupPoint.class),
        Mockito.anyBoolean())).thenReturn(new ItemPickupPointDataChangeEventModel());
    Mockito.doNothing().when(productAndItemSolrIndexerService).updateProductDetailsInSolr(anyList());
    Mockito.when(this.saveOperationService.saveProductWithoutUpdatingSolr(product, Collections.EMPTY_LIST,
        StringUtils.EMPTY, Collections.EMPTY_MAP)).thenReturn(product);
    Mockito.when(
        itemPickupPointService.existsCountByStoreIdAndItemSkuAndMarkForDeleteFalseAndCncBuyable(
            STORE_ID, ITEM_SKU, Constants.CNC)).thenReturn(Boolean.TRUE);
    Mockito.when(
        this.saveOperationService.saveItemsAndClearCacheWithoutUpdatingSolrWithoutPublishing(
            anyList())).thenReturn(Arrays.asList(ITEM));
    ItemViewConfigAndItemSkuRequest itemViewConfigAndItemSkuRequest2 = new ItemViewConfigAndItemSkuRequest();
    BeanUtils.copyProperties(itemViewConfigAndItemSkuRequest2, itemViewConfigAndItemSkuRequest);
    itemViewConfigAndItemSkuRequest2.setChannel(Constants.CNC);
    itemViewConfigAndItemSkuRequest2.setBuyable(true);
    itemViewConfigServiceImpl.updateItemViewConfigAndForceReview(STORE_ID,
        Arrays.asList(itemViewConfigAndItemSkuRequest, itemViewConfigAndItemSkuRequest2), true,
        false, PRODUCT_SKU, true);
    Mockito.verify(this.itemService).findByStoreIdAndItemSku(STORE_ID, ITEM_SKU);
    Mockito.verify(this.productService).findByStoreIdAndProductSkuIn(STORE_ID, List.of(PRODUCT_SKU));
    Mockito.verify(this.saveOperationService, times(2)).saveItemsAndClearCacheWithoutUpdatingSolrWithoutPublishing(anyList());
    Mockito.verify(saveAndPublishService)
        .publishListOfItems(Mockito.eq(Arrays.asList(ITEM)), anyList(), anyList(), anyBoolean());
    Mockito.verify(this.productAndItemSolrIndexerService, times(2)).updateSolrOnItemViewConfigChanges(anyList());
    Mockito.verify(saveAndPublishService).publishMerchantVoucherViewConfigChange(anyList(), anyList());
    Mockito.verify(itemPickupPointService).saveItemPickupPoint(Arrays.asList(itemPickupPoint));
    Mockito.verify(objectConverterService, times(2))
        .convertToItemPickupPointChangeEventModel(Mockito.any(ItemPickupPoint.class), Mockito.anyBoolean());
    Mockito.verify(productAndItemSolrIndexerService).updateProductDetailsInSolr(anyList());
    Mockito.verify(objectConverterService).overrideL4DetailsFromL5(Arrays.asList(ITEM), new ArrayList<>());
    Assertions.assertEquals(ItemChangeEventType.ARCHIVED_FLAG_CHANGE, ITEM.getItemChangeEventTypes().get(0));
    Mockito.verify(itemPickupPointSummaryService).updateExternalHistoryInPBP(auditTrailDtoListArgumentCaptor.capture());
    Mockito.verify(this.saveOperationService).saveProductWithoutUpdatingSolr(product, Collections.EMPTY_LIST,
        StringUtils.EMPTY, Collections.EMPTY_MAP);
    Mockito.verify(itemService).existsRecordForStoreIdAndProductSkuAndCncActivated(STORE_ID, PRODUCT_SKU, Boolean.TRUE);
    Mockito.verify(itemPickupPointService)
        .existsCountByStoreIdAndItemSkuAndMarkForDeleteFalseAndCncBuyable(STORE_ID, ITEM_SKU,
            Constants.CNC);
    Assertions.assertTrue(ITEM.isArchived());
    Assertions.assertEquals(1, auditTrailDtoListArgumentCaptor.getValue().size());
    ReflectionTestUtils.setField(itemViewConfigServiceImpl, "cncForWarehouseFeatureSwitch", false);
  }

  @Test
  public void updateItemViewConfigAndForceReviewTest_Cnc1P_l4Only()
      throws Exception {
    ReflectionTestUtils.setField(itemViewConfigServiceImpl, "cncForWarehouseFeatureSwitch", true);
    ITEM.setProductSku(PRODUCT_SKU);
    ITEM.setItemSku(ITEM_SKU);
    itemPickupPoint.setItemViewConfig(itemViewConfigSet);
    itemViewConfigAndItemSkuRequest.setItemBuyableSchedules(new ItemBuyableScheduleDTO());
    itemViewConfigAndItemSkuRequest.setChannel(null);
    itemViewConfigAndItemSkuRequest.setDiscoverable(true);
    Mockito.when(this.itemService.findByStoreIdAndItemSku(STORE_ID, ITEM_SKU)).thenReturn(ITEM);
    Mockito.when(this.saveOperationService.saveItemsAndClearCacheWithoutUpdatingSolr(Arrays.asList(ITEM), null, StringUtils.EMPTY))
        .thenReturn(Arrays.asList(ITEM));
    Mockito.when(itemService.existsRecordForStoreIdAndProductSkuAndCncActivated(STORE_ID, PRODUCT_SKU, Boolean.TRUE)).thenReturn(Boolean.FALSE);
    Mockito.when(this.productService.findByStoreIdAndProductSkuIn(STORE_ID, List.of(PRODUCT_SKU))).thenReturn(List.of(product));
    Mockito.doNothing().when(this.productAndItemSolrIndexerService).updateSolrOnItemViewConfigChanges(anyList());
    Mockito.doNothing().when(saveAndPublishService).publishMerchantVoucherViewConfigChange(anyList(), anyList());
    Mockito.when(itemPickupPointService.findByItemSkuAndPickupPointCode(STORE_ID, ITEM_SKU, PICKUP_POINT_CODE)).thenReturn(itemPickupPoint);
    Mockito.when(channelService.getDefaultChannel()).thenReturn(CHANNEL);
    Mockito.when(objectConverterService.convertToItemPickupPointChangeEventModel(Mockito.any(ItemPickupPoint.class),
        Mockito.anyBoolean())).thenReturn(new ItemPickupPointDataChangeEventModel());
    Mockito.when(
        itemPickupPointService.existsCountByStoreIdAndItemSkuAndMarkForDeleteFalseAndCncBuyable(
            STORE_ID, ITEM_SKU, Constants.CNC)).thenReturn(Boolean.TRUE);
    Mockito.when(
        this.saveOperationService.saveItemsAndClearCacheWithoutUpdatingSolrWithoutPublishing(
            anyList())).thenReturn(Arrays.asList(ITEM));
    ItemViewConfigAndItemSkuRequest itemViewConfigAndItemSkuRequest2 = new ItemViewConfigAndItemSkuRequest();
    BeanUtils.copyProperties(itemViewConfigAndItemSkuRequest2, itemViewConfigAndItemSkuRequest);
    itemViewConfigAndItemSkuRequest2.setChannel(Constants.CNC);
    itemViewConfigAndItemSkuRequest2.setBuyable(true);
    itemViewConfigServiceImpl.updateItemViewConfigAndForceReview(STORE_ID,
        Arrays.asList(itemViewConfigAndItemSkuRequest, itemViewConfigAndItemSkuRequest2), true,
        false, PRODUCT_SKU, true);
    Mockito.verify(this.itemService).findByStoreIdAndItemSku(STORE_ID, ITEM_SKU);
    Mockito.verify(itemService).existsRecordForStoreIdAndProductSkuAndCncActivated(STORE_ID, PRODUCT_SKU, Boolean.TRUE);
    Mockito.verify(this.productService).findByStoreIdAndProductSkuIn(STORE_ID, List.of(PRODUCT_SKU));
    Mockito.verify(this.saveOperationService, times(2)).saveItemsAndClearCacheWithoutUpdatingSolrWithoutPublishing(anyList());
    Mockito.verify(saveAndPublishService)
        .publishListOfItems(Mockito.eq(Arrays.asList(ITEM)), anyList(), anyList(), anyBoolean());
    Mockito.verify(this.productAndItemSolrIndexerService, times(2)).updateSolrOnItemViewConfigChanges(anyList());
    Mockito.verify(saveAndPublishService).publishMerchantVoucherViewConfigChange(anyList(), anyList());
    Mockito.verify(itemPickupPointService).saveItemPickupPoint(Arrays.asList(itemPickupPoint));
    Mockito.verify(objectConverterService, times(2))
        .convertToItemPickupPointChangeEventModel(Mockito.any(ItemPickupPoint.class), Mockito.anyBoolean());
    Mockito.verify(objectConverterService).overrideL4DetailsFromL5(Arrays.asList(ITEM), new ArrayList<>());
    Assertions.assertEquals(ItemChangeEventType.ARCHIVED_FLAG_CHANGE, ITEM.getItemChangeEventTypes().get(0));
    Mockito.verify(itemPickupPointSummaryService).updateExternalHistoryInPBP(auditTrailDtoListArgumentCaptor.capture());
    Mockito.verify(itemPickupPointService)
        .existsCountByStoreIdAndItemSkuAndMarkForDeleteFalseAndCncBuyable(STORE_ID, ITEM_SKU,
            Constants.CNC);
    Assertions.assertTrue(ITEM.isArchived());
    Assertions.assertEquals(1, auditTrailDtoListArgumentCaptor.getValue().size());
    ReflectionTestUtils.setField(itemViewConfigServiceImpl, "cncForWarehouseFeatureSwitch", false);
  }

  @Test
  public void updateItemViewConfigAndForceReviewTest_Cnc1P_l4Only_productNotFound()
      throws Exception {
    ReflectionTestUtils.setField(itemViewConfigServiceImpl, "cncForWarehouseFeatureSwitch", true);
    ITEM.setProductSku(PRODUCT_SKU);
    ITEM.setItemSku(ITEM_SKU);
    itemPickupPoint.setItemViewConfig(itemViewConfigSet);
    itemViewConfigAndItemSkuRequest.setItemBuyableSchedules(new ItemBuyableScheduleDTO());
    itemViewConfigAndItemSkuRequest.setChannel(null);
    itemViewConfigAndItemSkuRequest.setDiscoverable(true);
    Mockito.when(this.itemService.findByStoreIdAndItemSku(STORE_ID, ITEM_SKU)).thenReturn(ITEM);
    Mockito.when(this.saveOperationService.saveItemsAndClearCacheWithoutUpdatingSolr(Arrays.asList(ITEM), null, StringUtils.EMPTY))
        .thenReturn(Arrays.asList(ITEM));
    Mockito.when(this.productService.findByStoreIdAndProductSkuIn(STORE_ID, List.of(PRODUCT_SKU))).thenReturn(List.of());
    Mockito.doNothing().when(this.productAndItemSolrIndexerService).updateSolrOnItemViewConfigChanges(anyList());
    Mockito.doNothing().when(saveAndPublishService).publishMerchantVoucherViewConfigChange(anyList(), anyList());
    Mockito.when(itemPickupPointService.findByItemSkuAndPickupPointCode(STORE_ID, ITEM_SKU, PICKUP_POINT_CODE)).thenReturn(itemPickupPoint);
    Mockito.when(channelService.getDefaultChannel()).thenReturn(CHANNEL);
    Mockito.when(objectConverterService.convertToItemPickupPointChangeEventModel(Mockito.any(ItemPickupPoint.class),
        Mockito.anyBoolean())).thenReturn(new ItemPickupPointDataChangeEventModel());
    Mockito.when(
        itemPickupPointService.existsCountByStoreIdAndItemSkuAndMarkForDeleteFalseAndCncBuyable(
            STORE_ID, ITEM_SKU, Constants.CNC)).thenReturn(Boolean.TRUE);
    Mockito.when(
        this.saveOperationService.saveItemsAndClearCacheWithoutUpdatingSolrWithoutPublishing(
            anyList())).thenReturn(Arrays.asList(ITEM));
    ItemViewConfigAndItemSkuRequest itemViewConfigAndItemSkuRequest2 = new ItemViewConfigAndItemSkuRequest();
    BeanUtils.copyProperties(itemViewConfigAndItemSkuRequest2, itemViewConfigAndItemSkuRequest);
    itemViewConfigAndItemSkuRequest2.setChannel(Constants.CNC);
    itemViewConfigAndItemSkuRequest2.setBuyable(true);
    itemViewConfigServiceImpl.updateItemViewConfigAndForceReview(STORE_ID,
        Arrays.asList(itemViewConfigAndItemSkuRequest, itemViewConfigAndItemSkuRequest2), true,
        false, PRODUCT_SKU, true);
    Mockito.verify(this.itemService).findByStoreIdAndItemSku(STORE_ID, ITEM_SKU);
    Mockito.verify(this.productService).findByStoreIdAndProductSkuIn(STORE_ID, List.of(PRODUCT_SKU));
    Mockito.verify(this.saveOperationService, times(2)).saveItemsAndClearCacheWithoutUpdatingSolrWithoutPublishing(anyList());
    Mockito.verify(saveAndPublishService)
        .publishListOfItems(Mockito.eq(Arrays.asList(ITEM)), anyList(), anyList(), anyBoolean());
    Mockito.verify(this.productAndItemSolrIndexerService, times(2)).updateSolrOnItemViewConfigChanges(anyList());
    Mockito.verify(saveAndPublishService).publishMerchantVoucherViewConfigChange(anyList(), anyList());
    Mockito.verify(itemPickupPointService).saveItemPickupPoint(Arrays.asList(itemPickupPoint));
    Mockito.verify(objectConverterService, times(2))
        .convertToItemPickupPointChangeEventModel(Mockito.any(ItemPickupPoint.class), Mockito.anyBoolean());
    Mockito.verify(objectConverterService).overrideL4DetailsFromL5(Arrays.asList(ITEM), new ArrayList<>());
    Assertions.assertEquals(ItemChangeEventType.ARCHIVED_FLAG_CHANGE, ITEM.getItemChangeEventTypes().get(0));
    Mockito.verify(itemPickupPointSummaryService).updateExternalHistoryInPBP(auditTrailDtoListArgumentCaptor.capture());
    Mockito.verify(itemPickupPointService)
        .existsCountByStoreIdAndItemSkuAndMarkForDeleteFalseAndCncBuyable(STORE_ID, ITEM_SKU,
            Constants.CNC);
    Assertions.assertTrue(ITEM.isArchived());
    Assertions.assertEquals(1, auditTrailDtoListArgumentCaptor.getValue().size());
    ReflectionTestUtils.setField(itemViewConfigServiceImpl, "cncForWarehouseFeatureSwitch", false);
  }

  @Test
  public void updateItemViewConfigAndForceReviewTest_Cnc1P_noChangeL3AndL4()
      throws Exception {
    ReflectionTestUtils.setField(itemViewConfigServiceImpl, "cncForWarehouseFeatureSwitch", true);
    ITEM.setProductSku(PRODUCT_SKU);
    ITEM.setItemSku(ITEM_SKU);
    itemPickupPoint.setItemViewConfig(itemViewConfigSet);
    itemViewConfigAndItemSkuRequest.setItemBuyableSchedules(new ItemBuyableScheduleDTO());
    itemViewConfigAndItemSkuRequest.setChannel(null);
    itemViewConfigAndItemSkuRequest.setDiscoverable(true);
    Mockito.when(this.itemService.findByStoreIdAndItemSku(STORE_ID, ITEM_SKU)).thenReturn(ITEM);
    Mockito.when(this.saveOperationService.saveItemsAndClearCacheWithoutUpdatingSolr(Arrays.asList(ITEM), null, StringUtils.EMPTY))
        .thenReturn(Arrays.asList(ITEM));
    Mockito.when(this.productService.findByStoreIdAndProductSkuIn(STORE_ID, List.of(PRODUCT_SKU))).thenReturn(List.of());
    Mockito.doNothing().when(this.productAndItemSolrIndexerService).updateSolrOnItemViewConfigChanges(anyList());
    Mockito.doNothing().when(saveAndPublishService).publishMerchantVoucherViewConfigChange(anyList(), anyList());
    Mockito.when(itemPickupPointService.findByItemSkuAndPickupPointCode(STORE_ID, ITEM_SKU, PICKUP_POINT_CODE)).thenReturn(itemPickupPoint);
    Mockito.when(channelService.getDefaultChannel()).thenReturn(CHANNEL);
    Mockito.when(objectConverterService.convertToItemPickupPointChangeEventModel(Mockito.any(ItemPickupPoint.class),
        Mockito.anyBoolean())).thenReturn(new ItemPickupPointDataChangeEventModel());
    Mockito.when(
        itemPickupPointService.existsCountByStoreIdAndItemSkuAndMarkForDeleteFalseAndCncBuyable(
            STORE_ID, ITEM_SKU, Constants.CNC)).thenReturn(Boolean.FALSE);
    Mockito.when(
        this.saveOperationService.saveItemsAndClearCacheWithoutUpdatingSolrWithoutPublishing(
            anyList())).thenReturn(Arrays.asList(ITEM));
    ItemViewConfigAndItemSkuRequest itemViewConfigAndItemSkuRequest2 = new ItemViewConfigAndItemSkuRequest();
    BeanUtils.copyProperties(itemViewConfigAndItemSkuRequest2, itemViewConfigAndItemSkuRequest);
    itemViewConfigAndItemSkuRequest2.setChannel(Constants.CNC);
    itemViewConfigAndItemSkuRequest2.setBuyable(true);
    itemViewConfigServiceImpl.updateItemViewConfigAndForceReview(STORE_ID,
        Arrays.asList(itemViewConfigAndItemSkuRequest, itemViewConfigAndItemSkuRequest2), true,
        false, PRODUCT_SKU, true);
    Mockito.verify(this.itemService).findByStoreIdAndItemSku(STORE_ID, ITEM_SKU);
    Mockito.verify(this.saveOperationService).saveItemsAndClearCacheWithoutUpdatingSolrWithoutPublishing(anyList());
    Mockito.verify(saveAndPublishService)
        .publishListOfItems(Mockito.eq(Arrays.asList(ITEM)), anyList(), anyList(), anyBoolean());
    Mockito.verify(this.productAndItemSolrIndexerService).updateSolrOnItemViewConfigChanges(anyList());
    Mockito.verify(saveAndPublishService).publishMerchantVoucherViewConfigChange(anyList(), anyList());
    Mockito.verify(itemPickupPointService).saveItemPickupPoint(Arrays.asList(itemPickupPoint));
    Mockito.verify(objectConverterService, times(2))
        .convertToItemPickupPointChangeEventModel(Mockito.any(ItemPickupPoint.class),
            Mockito.anyBoolean());
    Mockito.verify(objectConverterService).overrideL4DetailsFromL5(Arrays.asList(ITEM), new ArrayList<>());
    Assertions.assertEquals(ItemChangeEventType.ARCHIVED_FLAG_CHANGE, ITEM.getItemChangeEventTypes().get(0));
    Mockito.verify(itemPickupPointSummaryService).updateExternalHistoryInPBP(auditTrailDtoListArgumentCaptor.capture());
    Mockito.verify(itemPickupPointService)
        .existsCountByStoreIdAndItemSkuAndMarkForDeleteFalseAndCncBuyable(STORE_ID, ITEM_SKU,
            Constants.CNC);
    Assertions.assertTrue(ITEM.isArchived());
    Assertions.assertEquals(1, auditTrailDtoListArgumentCaptor.getValue().size());
    ReflectionTestUtils.setField(itemViewConfigServiceImpl, "cncForWarehouseFeatureSwitch", false);
  }

  @Test
  public void updateItemViewConfigAndForceReviewTest_Cnc1P_nullItem()
      throws Exception {
    ReflectionTestUtils.setField(itemViewConfigServiceImpl, "cncForWarehouseFeatureSwitch", true);
    ITEM.setProductSku(PRODUCT_SKU);
    ITEM.setItemSku(ITEM_SKU);
    itemPickupPoint.setItemViewConfig(itemViewConfigSet);
    itemViewConfigAndItemSkuRequest.setItemBuyableSchedules(new ItemBuyableScheduleDTO());
    itemViewConfigAndItemSkuRequest.setChannel(null);
    itemViewConfigAndItemSkuRequest.setDiscoverable(true);
    Mockito.when(this.itemService.findByStoreIdAndItemSku(STORE_ID, ITEM_SKU)).thenReturn(ITEM);
    Mockito.when(this.saveOperationService.saveItemsAndClearCacheWithoutUpdatingSolr(Arrays.asList(ITEM), null, StringUtils.EMPTY))
        .thenReturn(Arrays.asList(ITEM));
    Mockito.when(this.productService.findByStoreIdAndProductSkuIn(STORE_ID, List.of(PRODUCT_SKU))).thenReturn(List.of());
    Mockito.doNothing().when(this.productAndItemSolrIndexerService).updateSolrOnItemViewConfigChanges(anyList());
    Mockito.doNothing().when(saveAndPublishService).publishMerchantVoucherViewConfigChange(anyList(), anyList());
    Mockito.when(itemPickupPointService.findByItemSkuAndPickupPointCode(STORE_ID, ITEM_SKU, PICKUP_POINT_CODE)).thenReturn(itemPickupPoint);
    Mockito.when(channelService.getDefaultChannel()).thenReturn(CHANNEL);
    Mockito.when(objectConverterService.convertToItemPickupPointChangeEventModel(Mockito.any(ItemPickupPoint.class),
        Mockito.anyBoolean())).thenReturn(new ItemPickupPointDataChangeEventModel());
    Item item = new Item();
    BeanUtils.copyProperties(item, ITEM);
    item.setItemSku(ITEM_SKU_2);
    Mockito.when(
        this.saveOperationService.saveItemsAndClearCacheWithoutUpdatingSolrWithoutPublishing(
            anyList())).thenReturn(Arrays.asList(item));
    ItemViewConfigAndItemSkuRequest itemViewConfigAndItemSkuRequest2 = new ItemViewConfigAndItemSkuRequest();
    BeanUtils.copyProperties(itemViewConfigAndItemSkuRequest2, itemViewConfigAndItemSkuRequest);
    itemViewConfigAndItemSkuRequest2.setChannel(Constants.CNC);
    itemViewConfigAndItemSkuRequest2.setBuyable(true);
    itemViewConfigServiceImpl.updateItemViewConfigAndForceReview(STORE_ID,
        Arrays.asList(itemViewConfigAndItemSkuRequest, itemViewConfigAndItemSkuRequest2), true,
        false, PRODUCT_SKU, true);
    Mockito.verify(this.itemService).findByStoreIdAndItemSku(STORE_ID, ITEM_SKU);
    Mockito.verify(this.saveOperationService).saveItemsAndClearCacheWithoutUpdatingSolrWithoutPublishing(anyList());
    Mockito.verify(saveAndPublishService)
        .publishListOfItems(Mockito.eq(Arrays.asList(ITEM)), anyList(), anyList(), anyBoolean());
    Mockito.verify(this.productAndItemSolrIndexerService).updateSolrOnItemViewConfigChanges(anyList());
    Mockito.verify(saveAndPublishService).publishMerchantVoucherViewConfigChange(anyList(), anyList());
    Mockito.verify(itemPickupPointService).saveItemPickupPoint(Arrays.asList(itemPickupPoint));
    Mockito.verify(objectConverterService, times(2))
        .convertToItemPickupPointChangeEventModel(Mockito.any(ItemPickupPoint.class),
            Mockito.anyBoolean());
    Mockito.verify(objectConverterService).overrideL4DetailsFromL5(Arrays.asList(ITEM), new ArrayList<>());
    Assertions.assertEquals(ItemChangeEventType.ARCHIVED_FLAG_CHANGE, ITEM.getItemChangeEventTypes().get(0));
    Mockito.verify(itemPickupPointSummaryService).updateExternalHistoryInPBP(auditTrailDtoListArgumentCaptor.capture());
    Assertions.assertTrue(ITEM.isArchived());
    Assertions.assertEquals(1, auditTrailDtoListArgumentCaptor.getValue().size());
    ReflectionTestUtils.setField(itemViewConfigServiceImpl, "cncForWarehouseFeatureSwitch", false);
  }

  @Test
  public void updateItemViewConfigAndForceReviewTakeDownSwitchFalseTest() {
    ReflectionTestUtils.setField(itemViewConfigServiceImpl, "cncForWarehouseFeatureSwitch", true);
    ReflectionTestUtils.setField(itemViewConfigServiceImpl, "takeDownSwitch", false);
    ITEM.setProductSku(PRODUCT_SKU);
    ITEM.setItemSku(ITEM_SKU);
    itemPickupPoint.setItemViewConfig(itemViewConfigSet);
    itemPickupPoint.getAllItemViewConfigs().add(ItemViewConfig.builder().channel(Constants.CNC).isDiscoverable(true).build());
    itemViewConfigAndItemSkuRequest.setItemBuyableSchedules(new ItemBuyableScheduleDTO());
    itemViewConfigAndItemSkuRequest.setChannel(null);
    itemViewConfigAndItemSkuRequest.setDiscoverable(true);
    Mockito.when(this.itemService.findByStoreIdAndItemSku(STORE_ID, ITEM_SKU)).thenReturn(ITEM);
    Mockito.when(this.saveOperationService.saveItemsAndClearCacheWithoutUpdatingSolr(Arrays.asList(ITEM), null, StringUtils.EMPTY))
        .thenReturn(Arrays.asList(ITEM));
    Mockito.doNothing().when(this.productAndItemSolrIndexerService).updateSolrOnItemViewConfigChanges(Collections.singletonList(ITEM));
    Mockito.doNothing().when(saveAndPublishService).publishMerchantVoucherViewConfigChange(anyList(), anyList());
    Mockito.when(itemPickupPointService.findByItemSkuAndPickupPointCode(STORE_ID, ITEM_SKU, PICKUP_POINT_CODE)).thenReturn(itemPickupPoint);
    Mockito.when(channelService.getDefaultChannel()).thenReturn(CHANNEL);
    Mockito.when(objectConverterService.convertToItemPickupPointChangeEventModel(Mockito.any(ItemPickupPoint.class),
        Mockito.anyBoolean())).thenReturn(new ItemPickupPointDataChangeEventModel());
    itemViewConfigServiceImpl.updateItemViewConfigAndForceReview(STORE_ID, Arrays.asList(itemViewConfigAndItemSkuRequest), true,
        false, PRODUCT_SKU, false);
    Mockito.verify(this.itemService).findByStoreIdAndItemSku(STORE_ID, ITEM_SKU);
    Mockito.verify(this.saveOperationService).saveItemsAndClearCacheWithoutUpdatingSolrWithoutPublishing(Arrays.asList(ITEM));
    Mockito.verify(saveAndPublishService)
        .publishListOfItems(Mockito.eq(Arrays.asList(ITEM)), anyList(), anyList(), anyBoolean());
    Mockito.verify(this.productAndItemSolrIndexerService).updateSolrOnItemViewConfigChanges(Arrays.asList(ITEM));
    Mockito.verify(saveAndPublishService).publishMerchantVoucherViewConfigChange(anyList(), anyList());
    Mockito.verify(itemPickupPointService).saveItemPickupPoint(Arrays.asList(itemPickupPoint));
    Mockito.verify(objectConverterService)
        .convertToItemPickupPointChangeEventModel(Mockito.any(ItemPickupPoint.class), Mockito.anyBoolean());
    Mockito.verify(objectConverterService).overrideL4DetailsFromL5(Arrays.asList(ITEM), new ArrayList<>());
    Mockito.verify(itemPickupPointSummaryService).updateExternalHistoryInPBP(new ArrayList<>());
    Assertions.assertEquals(ItemChangeEventType.ARCHIVED_FLAG_CHANGE, ITEM.getItemChangeEventTypes().get(0));
    Assertions.assertTrue(ITEM.isArchived());
  }

  @Test
  public void updateItemViewConfigAndForceReviewItemPickupPointNullTest() {
    ITEM.setProductSku(PRODUCT_SKU);
    ITEM.setItemSku(ITEM_SKU);
    Item item2 = new Item();
    item2.setItemSku(ITEM_SKU_2);
    item2.setPermanentDelete(true);
    itemPickupPoint.setItemViewConfig(itemViewConfigSet);
    itemViewConfigAndItemSkuRequest.setItemBuyableSchedules(new ItemBuyableScheduleDTO());
    itemViewConfigAndItemSkuRequest.setChannel(null);
    itemViewConfigAndItemSkuRequest.setDiscoverable(true);
    ItemViewConfigAndItemSkuRequest itemViewConfigAndItemSkuRequest2 = new ItemViewConfigAndItemSkuRequest();
    itemViewConfigAndItemSkuRequest2.setItemSku(ITEM_SKU_2);
    Mockito.when(this.itemService.findByStoreIdAndItemSku(STORE_ID, ITEM_SKU)).thenReturn(ITEM);
    Mockito.when(this.itemService.findByStoreIdAndItemSku(STORE_ID, ITEM_SKU_2)).thenReturn(item2);
    Mockito.when(this.saveOperationService.saveItemsAndClearCacheWithoutUpdatingSolr(Arrays.asList(ITEM), null, StringUtils.EMPTY))
        .thenReturn(Arrays.asList(ITEM));
    Mockito.doNothing().when(this.productAndItemSolrIndexerService).updateSolrOnItemViewConfigChanges(Collections.singletonList(ITEM));
    Mockito.doNothing().when(saveAndPublishService).publishMerchantVoucherViewConfigChange(anyList(), anyList());
    Mockito.when(itemPickupPointService.findByItemSkuAndPickupPointCode(STORE_ID, ITEM_SKU, PICKUP_POINT_CODE)).thenReturn(null);
    Mockito.when(channelService.getDefaultChannel()).thenReturn(CHANNEL);
    Mockito.when(objectConverterService.convertToItemPickupPointChangeEventModel(Mockito.any(ItemPickupPoint.class),
        Mockito.anyBoolean())).thenReturn(new ItemPickupPointDataChangeEventModel());
    itemViewConfigServiceImpl.updateItemViewConfigAndForceReview(STORE_ID, Arrays.asList(itemViewConfigAndItemSkuRequest,
        itemViewConfigAndItemSkuRequest2), true, false, PRODUCT_SKU, false);
    Mockito.verify(this.itemService).findByStoreIdAndItemSku(STORE_ID, ITEM_SKU);
    Mockito.verify(this.itemService).findByStoreIdAndItemSku(STORE_ID, ITEM_SKU_2);
    Assertions.assertEquals(ItemChangeEventType.ARCHIVED_FLAG_CHANGE, ITEM.getItemChangeEventTypes().get(0));
    Assertions.assertTrue(ITEM.isArchived());
  }

  @Test
  public void updateItemViewConfigAndForceReviewEmptyItemViewConfigRequestTest() {
    ITEM.setProductSku(PRODUCT_SKU);
    ITEM.setItemSku(ITEM_SKU);
    itemPickupPoint.setItemViewConfig(itemViewConfigSet);
    itemViewConfigAndItemSkuRequest.setItemBuyableSchedules(new ItemBuyableScheduleDTO());
    itemViewConfigAndItemSkuRequest.setChannel(null);
    itemViewConfigAndItemSkuRequest.setDiscoverable(true);
    Mockito.when(this.itemService.findByStoreIdAndItemSku(STORE_ID, ITEM_SKU)).thenReturn(ITEM);
    Mockito.doNothing().when(this.productAndItemSolrIndexerService)
        .updateSolrOnItemViewConfigChanges(Collections.singletonList(ITEM));
    Mockito.doNothing().when(saveAndPublishService).publishMerchantVoucherViewConfigChange(anyList(), anyList());
    Mockito.when(itemPickupPointService.findByItemSkuAndPickupPointCode(STORE_ID, ITEM_SKU, PICKUP_POINT_CODE))
        .thenReturn(itemPickupPoint);
    Mockito.when(channelService.getDefaultChannel()).thenReturn(CHANNEL);
    Mockito.when(objectConverterService.convertToItemPickupPointChangeEventModel(Mockito.any(ItemPickupPoint.class),
        Mockito.anyBoolean())).thenReturn(new ItemPickupPointDataChangeEventModel());
    Mockito.when(itemService.findItemsByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU))
        .thenReturn(Arrays.asList(ITEM));
    itemViewConfigServiceImpl.updateItemViewConfigAndForceReview(STORE_ID, new ArrayList<>(), true, false, PRODUCT_SKU, false);
    Mockito.verify(itemService).findItemsByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU);
    Mockito.verify(this.saveOperationService)
        .saveItemsAndClearCacheWithoutUpdatingSolrWithoutPublishing(anyList());
    Mockito.verify(saveAndPublishService).publishListOfItems(anyList());
  }

  @Test
  public void updateItemViewConfigAndForceReviewEmptyItemViewConfigRequestTestForceReviewFalse() {
    List<Item> response =
        itemViewConfigServiceImpl.updateItemViewConfigAndForceReview(STORE_ID, new ArrayList<>(), false, false,
            PRODUCT_SKU, false);
    Assertions.assertEquals(new ArrayList<>(), response);
  }

  @Test
  public void updateItemViewConfigAndForceReviewEmptyItemViewConfigRequestTestEmptyProdcutSku() {
    List<Item> response =
        itemViewConfigServiceImpl.updateItemViewConfigAndForceReview(STORE_ID, new ArrayList<>(), false, false, StringUtils.EMPTY, false);
    Assertions.assertEquals(new ArrayList<>(), response);
  }

  @Test
  public void updateItemViewConfigAndForceReviewEmptyItemViewConfigRequestEmptyItemsTest() {
    ITEM.setProductSku(PRODUCT_SKU);
    ITEM.setItemSku(ITEM_SKU);
    itemPickupPoint.setItemViewConfig(itemViewConfigSet);
    itemViewConfigAndItemSkuRequest.setItemBuyableSchedules(new ItemBuyableScheduleDTO());
    itemViewConfigAndItemSkuRequest.setChannel(null);
    itemViewConfigAndItemSkuRequest.setDiscoverable(true);
    Mockito.when(this.itemService.findByStoreIdAndItemSku(STORE_ID, ITEM_SKU)).thenReturn(ITEM);
    Mockito.when(this.saveOperationService.saveItemsAndClearCacheWithoutUpdatingSolr(Arrays.asList(ITEM), null, StringUtils.EMPTY))
        .thenReturn(Arrays.asList(ITEM));
    Mockito.doNothing().when(this.productAndItemSolrIndexerService)
        .updateSolrOnItemViewConfigChanges(Collections.singletonList(ITEM));
    Mockito.doNothing().when(saveAndPublishService).publishMerchantVoucherViewConfigChange(anyList(), anyList());
    Mockito.when(itemPickupPointService.findByItemSkuAndPickupPointCode(STORE_ID, ITEM_SKU, PICKUP_POINT_CODE))
        .thenReturn(itemPickupPoint);
    Mockito.when(channelService.getDefaultChannel()).thenReturn(CHANNEL);
    Mockito.when(objectConverterService.convertToItemPickupPointChangeEventModel(Mockito.any(ItemPickupPoint.class),
        Mockito.anyBoolean())).thenReturn(new ItemPickupPointDataChangeEventModel());
    Mockito.when(itemService.findItemsByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU))
        .thenReturn(new ArrayList<>());
    itemViewConfigServiceImpl.updateItemViewConfigAndForceReview(STORE_ID, new ArrayList<>(), true, false, PRODUCT_SKU, false);
    Mockito.verify(itemService).findItemsByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU);
  }

  @Test
  public void updateItemViewConfigAndArchiveTest() {
    ReflectionTestUtils.setField(itemViewConfigServiceImpl, "setArchivedBeforeEditFlag", false);
    ITEM.setProductSku(PRODUCT_SKU);
    ITEM.setItemSku(ITEM_SKU);
    itemPickupPoint.setItemViewConfig(itemViewConfigSet);
    itemViewConfigAndItemSkuRequest.setItemBuyableSchedules(new ItemBuyableScheduleDTO());
    itemViewConfigAndItemSkuRequest.setChannel(null);
    itemViewConfigAndItemSkuRequest.setDiscoverable(true);
    Mockito.when(this.itemService.findByStoreIdAndItemSku(STORE_ID, ITEM_SKU)).thenReturn(ITEM);
    Mockito.when(this.saveOperationService.saveItemsAndClearCacheWithoutUpdatingSolr(Arrays.asList(ITEM), null, StringUtils.EMPTY))
        .thenReturn(Arrays.asList(ITEM));
    Mockito.doNothing().when(this.productAndItemSolrIndexerService).updateSolrOnItemViewConfigChanges(Collections.singletonList(ITEM));
    Mockito.doNothing().when(saveAndPublishService).publishMerchantVoucherViewConfigChange(anyList(), anyList());
    Mockito.when(itemPickupPointService.findByItemSkuAndPickupPointCode(STORE_ID, ITEM_SKU, PICKUP_POINT_CODE)).thenReturn(itemPickupPoint);
    Mockito.when(channelService.getDefaultChannel()).thenReturn(CHANNEL);
    Mockito.when(objectConverterService.convertToItemPickupPointChangeEventModel(Mockito.any(ItemPickupPoint.class),
        Mockito.anyBoolean())).thenReturn(new ItemPickupPointDataChangeEventModel());
    itemViewConfigServiceImpl.updateItemViewConfigAndForceReview(STORE_ID, Arrays.asList(itemViewConfigAndItemSkuRequest), false,
        true, PRODUCT_SKU, false);
    Mockito.verify(this.itemService).findByStoreIdAndItemSku(STORE_ID, ITEM_SKU);
    Mockito.verify(this.saveOperationService).saveItemsAndClearCacheWithoutUpdatingSolrWithoutPublishing(Arrays.asList(ITEM));
    Mockito.verify(saveAndPublishService)
        .publishListOfItems(Mockito.eq(Arrays.asList(ITEM)), anyList(), anyList(), anyBoolean());
    Mockito.verify(this.productAndItemSolrIndexerService).updateSolrOnItemViewConfigChanges(Arrays.asList(ITEM));
    Mockito.verify(saveAndPublishService).publishMerchantVoucherViewConfigChange(anyList(), anyList());
    Mockito.verify(itemPickupPointService).saveItemPickupPoint(Arrays.asList(itemPickupPoint));
    Mockito.verify(objectConverterService)
        .convertToItemPickupPointChangeEventModel(Mockito.any(ItemPickupPoint.class), Mockito.anyBoolean());
    Mockito.verify(objectConverterService).overrideL4DetailsFromL5(Arrays.asList(ITEM), new ArrayList<>());
    Mockito.verify(itemPickupPointSummaryService).updateExternalHistoryInPBP(new ArrayList<>());
    Assertions.assertEquals(ItemChangeEventType.ARCHIVED_FLAG_CHANGE, ITEM.getItemChangeEventTypes().get(0));
    Assertions.assertTrue(ITEM.isArchived());
  }

  @Test
  public void updateItemViewConfigAndForceReviewAndArchiveTest() {
    ITEM.setProductSku(PRODUCT_SKU);
    ITEM.setItemSku(ITEM_SKU);
    itemPickupPoint.setItemViewConfig(itemViewConfigSet);
    itemViewConfigAndItemSkuRequest.setItemBuyableSchedules(new ItemBuyableScheduleDTO());
    itemViewConfigAndItemSkuRequest.setChannel(null);
    itemViewConfigAndItemSkuRequest.setDiscoverable(true);
    Mockito.when(this.itemService.findByStoreIdAndItemSku(STORE_ID, ITEM_SKU)).thenReturn(ITEM);
    Mockito.when(this.saveOperationService.saveItemsAndClearCacheWithoutUpdatingSolr(Arrays.asList(ITEM), null, StringUtils.EMPTY))
        .thenReturn(Arrays.asList(ITEM));
    Mockito.doNothing().when(this.productAndItemSolrIndexerService).updateSolrOnItemViewConfigChanges(Collections.singletonList(ITEM));
    Mockito.doNothing().when(saveAndPublishService).publishMerchantVoucherViewConfigChange(anyList(), anyList());
    Mockito.when(itemPickupPointService.findByItemSkuAndPickupPointCode(STORE_ID, ITEM_SKU, PICKUP_POINT_CODE)).thenReturn(itemPickupPoint);
    Mockito.when(channelService.getDefaultChannel()).thenReturn(CHANNEL);
    Mockito.when(objectConverterService.convertToItemPickupPointChangeEventModel(Mockito.any(ItemPickupPoint.class),
        Mockito.anyBoolean())).thenReturn(new ItemPickupPointDataChangeEventModel());
    itemViewConfigServiceImpl.updateItemViewConfigAndForceReview(STORE_ID, Arrays.asList(itemViewConfigAndItemSkuRequest), true,
        true, StringUtils.EMPTY, false);
    Mockito.verify(this.itemService).findByStoreIdAndItemSku(STORE_ID, ITEM_SKU);
    Mockito.verify(this.saveOperationService).saveItemsAndClearCacheWithoutUpdatingSolrWithoutPublishing(Arrays.asList(ITEM));
    Mockito.verify(saveAndPublishService)
        .publishListOfItems(anyList(), anyList(), anyList(), anyBoolean());
    Mockito.verify(this.productAndItemSolrIndexerService).updateSolrOnItemViewConfigChanges(Arrays.asList(ITEM));
    Mockito.verify(saveAndPublishService).publishMerchantVoucherViewConfigChange(anyList(), anyList());
    Mockito.verify(itemPickupPointService).saveItemPickupPoint(Arrays.asList(itemPickupPoint));
    Mockito.verify(objectConverterService)
        .convertToItemPickupPointChangeEventModel(Mockito.any(ItemPickupPoint.class), Mockito.anyBoolean());
    Mockito.verify(objectConverterService).overrideL4DetailsFromL5(Arrays.asList(ITEM), new ArrayList<>());
    Mockito.verify(itemPickupPointSummaryService).updateExternalHistoryInPBP(new ArrayList<>());
    Assertions.assertEquals(ItemChangeEventType.ARCHIVED_FLAG_CHANGE, ITEM.getItemChangeEventTypes().get(0));
    Assertions.assertTrue(ITEM.isArchived());
  }

  @Test
  public void updateItemViewConfigAndForceReviewFalseTest() {
    ReflectionTestUtils.setField(itemViewConfigServiceImpl, "setArchivedBeforeEditFlag", true);
    ITEM.setProductSku(PRODUCT_SKU);
    ITEM.setItemSku(ITEM_SKU);
    itemViewConfigAndItemSkuRequest.setItemDiscoverableSchedules(new ItemDiscoverableScheduleDTO());
    Mockito.when(this.itemService.findByStoreIdAndItemSku(STORE_ID, ITEM_SKU)).thenReturn(ITEM);
    Mockito.when(this.productHelperService.updateItemViewConfigForExistingChannel(ITEM, itemViewConfigMap.get(ITEM_SKU)))
        .thenReturn(ITEM);
    Mockito.when(this.saveOperationService.saveItemsAndClearCacheWithoutUpdatingSolr(Arrays.asList(ITEM), null, StringUtils.EMPTY))
        .thenReturn(Arrays.asList(ITEM));
    Mockito.when(objectConverterService.convertToItemPickupPointChangeEventModel(Mockito.any(ItemPickupPoint.class),
        Mockito.anyBoolean())).thenReturn(new ItemPickupPointDataChangeEventModel());
    Mockito.doNothing().when(this.productAndItemSolrIndexerService).updateSolrOnItemViewConfigChanges(Collections.singletonList(ITEM));
    Mockito.doNothing().when(saveAndPublishService).publishMerchantVoucherViewConfigChange(anyList(), anyList());
    Mockito.when(itemPickupPointService.findByItemSkuAndPickupPointCode(STORE_ID, ITEM_SKU, PICKUP_POINT_CODE)).thenReturn(itemPickupPoint);
    itemViewConfigServiceImpl.updateItemViewConfigAndForceReview(STORE_ID, Arrays.asList(itemViewConfigAndItemSkuRequest, itemViewConfigAndItemSkuRequest), false,
        false, StringUtils.EMPTY, false);
    Mockito.verify(this.itemService).findByStoreIdAndItemSku(STORE_ID, ITEM_SKU);
    Mockito.verify(this.saveOperationService).saveItemsAndClearCacheWithoutUpdatingSolrWithoutPublishing(Arrays.asList(ITEM));
    Mockito.verify(saveAndPublishService)
        .publishListOfItems(anyList(), anyList(), anyList(), anyBoolean());
    Mockito.verify(objectConverterService, Mockito.times(2))
        .convertToItemPickupPointChangeEventModel(Mockito.any(ItemPickupPoint.class), Mockito.anyBoolean());
    Mockito.verify(this.productAndItemSolrIndexerService).updateSolrOnItemViewConfigChanges(Arrays.asList(ITEM));
    Mockito.verify(saveAndPublishService).publishMerchantVoucherViewConfigChange(anyList(), anyList());
    Mockito.verify(objectConverterService).overrideL4DetailsFromL5(Arrays.asList(ITEM), new ArrayList<>());
    Mockito.verify(itemPickupPointSummaryService).updateExternalHistoryInPBP(new ArrayList<>());
    Assertions.assertEquals(ITEM.isArchivedBeforeEdit(), ITEM.isArchived());
  }

  @Test
  public void updateItemViewConfigAndForceReviewFalseTest2() {
    ReflectionTestUtils.setField(itemViewConfigServiceImpl, "setArchivedBeforeEditFlag", false);
    ITEM.setProductSku(PRODUCT_SKU);
    ITEM.setItemSku(ITEM_SKU);
    itemViewConfigAndItemSkuRequest.setItemDiscoverableSchedules(new ItemDiscoverableScheduleDTO());
    Mockito.when(this.itemService.findByStoreIdAndItemSku(STORE_ID, ITEM_SKU)).thenReturn(ITEM);
    Mockito.when(this.productHelperService.updateItemViewConfigForExistingChannel(ITEM, itemViewConfigMap.get(ITEM_SKU)))
        .thenReturn(ITEM);
    Mockito.when(this.saveOperationService.saveItemsAndClearCacheWithoutUpdatingSolr(Arrays.asList(ITEM), null, StringUtils.EMPTY))
        .thenReturn(Arrays.asList(ITEM));
    Mockito.when(objectConverterService.convertToItemPickupPointChangeEventModel(Mockito.any(ItemPickupPoint.class),
        Mockito.anyBoolean())).thenReturn(new ItemPickupPointDataChangeEventModel());
    Mockito.doNothing().when(this.productAndItemSolrIndexerService).updateSolrOnItemViewConfigChanges(Collections.singletonList(ITEM));
    Mockito.doNothing().when(saveAndPublishService).publishMerchantVoucherViewConfigChange(anyList(), anyList());
    Mockito.when(itemPickupPointService.findByItemSkuAndPickupPointCode(STORE_ID, ITEM_SKU, PICKUP_POINT_CODE)).thenReturn(itemPickupPoint);
    itemViewConfigServiceImpl.updateItemViewConfigAndForceReview(STORE_ID, Arrays.asList(itemViewConfigAndItemSkuRequest, itemViewConfigAndItemSkuRequest), false,
        false, StringUtils.EMPTY, false);
    Mockito.verify(this.itemService).findByStoreIdAndItemSku(STORE_ID, ITEM_SKU);
    Mockito.verify(this.saveOperationService).saveItemsAndClearCacheWithoutUpdatingSolrWithoutPublishing(Arrays.asList(ITEM));
    Mockito.verify(saveAndPublishService)
        .publishListOfItems(anyList(), anyList(), anyList(), anyBoolean());
    Mockito.verify(objectConverterService, Mockito.times(2))
        .convertToItemPickupPointChangeEventModel(Mockito.any(ItemPickupPoint.class), Mockito.anyBoolean());
    Mockito.verify(this.productAndItemSolrIndexerService).updateSolrOnItemViewConfigChanges(Arrays.asList(ITEM));
    Mockito.verify(saveAndPublishService).publishMerchantVoucherViewConfigChange(anyList(), anyList());
    Mockito.verify(objectConverterService).overrideL4DetailsFromL5(Arrays.asList(ITEM), new ArrayList<>());
    Mockito.verify(itemPickupPointSummaryService).updateExternalHistoryInPBP(new ArrayList<>());
    Assertions.assertFalse(ITEM.isArchived());
  }

  @Test
  public void updateItemViewConfigAndForceReviewFalseEmptyListTest() {
    ITEM.setProductSku(PRODUCT_SKU);
    ITEM.setItemSku(ITEM_SKU);
    Mockito.when(this.itemService.findByStoreIdAndItemSku(STORE_ID, ITEM_SKU)).thenReturn(ITEM);
    Mockito.when(this.productHelperService.updateItemViewConfigForExistingChannel(ITEM, itemViewConfigMap.get(ITEM_SKU)))
        .thenReturn(ITEM);
    Mockito.when(this.saveOperationService.saveItemsAndClearCacheWithoutUpdatingSolr(Arrays.asList(ITEM), null, StringUtils.EMPTY))
        .thenReturn(Arrays.asList(ITEM));
    Mockito.doNothing().when(this.productAndItemSolrIndexerService).updateSolrOnItemViewConfigChanges(Collections.singletonList(ITEM));
    Mockito.doNothing().when(saveAndPublishService).publishMerchantVoucherViewConfigChange(anyList(), anyList());
    itemViewConfigServiceImpl.updateItemViewConfigAndForceReview(STORE_ID, new ArrayList<>(), false, false, StringUtils.EMPTY, false);
  }

  @Test
  public void updateItemViewConfigAndForceReviewTest_nullItem() {
    ITEM.setProductSku(PRODUCT_SKU);
    Mockito.when(this.itemService.findByStoreIdAndItemSku(STORE_ID, ITEM_SKU)).thenReturn(null);
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> itemViewConfigServiceImpl.updateItemViewConfigAndForceReview(STORE_ID, Arrays.asList(itemViewConfigAndItemSkuRequest), true,
          false, StringUtils.EMPTY, false));
    } finally {
      Mockito.verify(this.itemService).findByStoreIdAndItemSku(STORE_ID, ITEM_SKU);
    }
  }

  @Test
  public void updateItemViewConfigWithItemStatusTest() throws Exception {
    ITEM.setArchived(true);
    Mockito.when(this.itemService.findByStoreIdAndItemSku(STORE_ID, ITEM_SKU)).thenReturn(ITEM);
    itemViewConfigServiceImpl.updateProductItemViewConfig(STORE_ID, ITEM_SKU, false, false);
    Mockito.verify(this.itemService).findByStoreIdAndItemSku(STORE_ID, ITEM_SKU);
  }

  @Test
  public void updateItemViewConfigWithItemStatusDeletedTest() throws Exception {
    ITEM.setMarkForDelete(true);
    ITEM.setItemSku(ITEM_SKU);
    when(productHelperService.updateItemViewConfigForExistingChannelItemPickupPoint(itemPickupPoint,
        ITEM_VIEW_CONFIG)).thenReturn(true);
    when(itemPickupPointService.findByItemSkuAndDelivery(STORE_ID,
        ITEM_SKU)).thenReturn(itemPickupPoint);
    when(itemPickupPointService.saveItemPickupPoint(Mockito.any(ItemPickupPoint.class))).thenReturn(new ItemPickupPoint());
    doNothing().when(cacheEvictHelperService)
        .evictItemPickupPointData(Mockito.anyString(), Mockito.any(ItemPickupPoint.class), Mockito.anyString());
    Mockito.when(this.itemService.findByStoreIdAndItemSku(STORE_ID, ITEM_SKU)).thenReturn(ITEM);
    itemViewConfigServiceImpl.updateProductItemViewConfig(STORE_ID, ITEM_SKU, false, false);
    verify(itemService).findByStoreIdAndItemSku(STORE_ID, ITEM_SKU);
    verify(productHelperService).updateItemViewConfigForExistingChannelItemPickupPoint(itemPickupPoint,
        ITEM_VIEW_CONFIG);
    verify(productAndItemSolrIndexerService).updateSolrOnItemViewConfigChangesByItemPickupPoint(anyList());
    verify(saveAndPublishService).publishMerchantVoucherViewConfigChange(Mockito.any(ItemVo.class));
    verify(itemPickupPointService).findByItemSkuAndDelivery(STORE_ID, ITEM_SKU);
    verify(itemPickupPointService).saveItemPickupPoint(Mockito.any(ItemPickupPoint.class));
    verify(cacheEvictHelperService).evictItemPickupPointData(Mockito.isNull(), Mockito.any(ItemPickupPoint.class),
        Mockito.isNull());
    verify(cacheEvictHelperService).evictItemPickupPointData(Mockito.isNull(), Mockito.any(ItemPickupPoint.class),
        Mockito.isNull());
    verify(cacheEvictHelperService).evictItemData(Mockito.isNull(), Mockito.any(Item.class));
    verify(cacheEvictHelperService).evictItemPickupPointData(Mockito.isNull(), Mockito.any(ItemPickupPoint.class),
        Mockito.isNull());
    verify(cacheEvictHelperService).evictItemData(Mockito.isNull(), Mockito.any(Item.class));
    verify(saveAndPublishService).publishItemPickupPointDataChangeEvent(Mockito.any(),Mockito.any(),
      eq(Collections.EMPTY_MAP));
  }

  @Test
  public void updateItemViewConfigWithItemStatusBuyableFalseTest() throws Exception {
    ITEM.setArchived(false);
    ITEM.getItemViewConfigs().stream().findFirst().get().setBuyable(false);
    ITEM.getItemViewConfigs().stream().findFirst().get().setDiscoverable(true);
    ITEM.setItemSku(ITEM_SKU);
    when(productHelperService.updateItemViewConfigForExistingChannelItemPickupPoint(itemPickupPoint,
        ITEM_VIEW_CONFIG)).thenReturn(true);
    when(itemPickupPointService.findByItemSkuAndDelivery(STORE_ID,
        ITEM_SKU)).thenReturn(itemPickupPoint);
    when(itemPickupPointService.saveItemPickupPoint(Mockito.any(ItemPickupPoint.class))).thenReturn(new ItemPickupPoint());
    doNothing().when(cacheEvictHelperService)
        .evictItemPickupPointData(Mockito.anyString(), Mockito.any(ItemPickupPoint.class), Mockito.anyString());
    Mockito.when(this.itemService.findByStoreIdAndItemSku(STORE_ID, ITEM_SKU)).thenReturn(ITEM);
    Mockito.when(this.itemService.findByStoreIdAndItemSku(STORE_ID, ITEM_SKU)).thenReturn(ITEM);
    itemViewConfigServiceImpl.updateProductItemViewConfig(STORE_ID, ITEM_SKU, false, false);
    verify(itemService).findByStoreIdAndItemSku(STORE_ID, ITEM_SKU);
    verify(productHelperService).updateItemViewConfigForExistingChannelItemPickupPoint(itemPickupPoint,
        ITEM_VIEW_CONFIG);
    verify(productAndItemSolrIndexerService).updateSolrOnItemViewConfigChangesByItemPickupPoint(anyList());
    verify(saveAndPublishService).publishMerchantVoucherViewConfigChange(Mockito.any(ItemVo.class));
    verify(itemPickupPointService).findByItemSkuAndDelivery(STORE_ID, ITEM_SKU);
    verify(itemPickupPointService).saveItemPickupPoint(Mockito.any(ItemPickupPoint.class));
    verify(cacheEvictHelperService).evictItemPickupPointData(Mockito.isNull(), Mockito.any(ItemPickupPoint.class),
        Mockito.isNull());
    verify(cacheEvictHelperService).evictItemPickupPointData(Mockito.isNull(), Mockito.any(ItemPickupPoint.class),
        Mockito.isNull());
    verify(cacheEvictHelperService).evictItemData(Mockito.isNull(), Mockito.any(Item.class));
    verify(cacheEvictHelperService).evictItemPickupPointData(Mockito.isNull(), Mockito.any(ItemPickupPoint.class),
        Mockito.isNull());
    verify(cacheEvictHelperService).evictItemData(Mockito.isNull(), Mockito.any(Item.class));
    verify(saveAndPublishService).publishItemPickupPointDataChangeEvent(Mockito.any(),Mockito.any(),
      eq(Collections.EMPTY_MAP));
  }

  @Test
  public void updateItemViewConfigWithItemStatusBuyableDiscoverableFalseTest() throws Exception {
    ITEM.setArchived(false);
    ITEM.getItemViewConfigs().stream().findFirst().get().setBuyable(false);
    ITEM.getItemViewConfigs().stream().findFirst().get().setDiscoverable(false);
    ITEM.setItemSku(ITEM_SKU);
    Mockito.when(this.itemService.findByStoreIdAndItemSku(STORE_ID, ITEM_SKU)).thenReturn(ITEM);
    itemViewConfigServiceImpl.updateProductItemViewConfig(STORE_ID, ITEM_SKU, false, false);
    Mockito.verify(this.itemService).findByStoreIdAndItemSku(STORE_ID, ITEM_SKU);
  }

  @Test
  public void updateItemViewConfigWithMPPTest() throws Exception {
    when(productHelperService.updateItemViewConfigForExistingChannelItemPickupPoint(itemPickupPoint,
        ITEM_VIEW_CONFIG)).thenReturn(false);
    when(itemPickupPointService.findByItemSkuAndDelivery(STORE_ID,
      ITEM_SKU)).thenReturn(itemPickupPoint);
    Item item  = new Item();
    item.setItemSku(ITEM_SKU);
    item.setMarkForDelete(false);
    item.setProductSku(PRODUCT_SKU);
    when(this.itemService.findByStoreIdAndItemSkuAndMarkForDeleteFalse(anyString(),anyString())).thenReturn(item);
    itemViewConfigServiceImpl.updateItemViewConfig(STORE_ID, ITEM_SKU, ITEM_VIEW_CONFIG);
    verify(itemService).findByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, ITEM_SKU);
    verify(productHelperService).updateItemViewConfigForExistingChannelItemPickupPoint(itemPickupPoint,
      ITEM_VIEW_CONFIG);
    verify(saveAndPublishService).publishMerchantVoucherViewConfigChange(Mockito.any(ItemVo.class));
    verify(itemPickupPointService).findByItemSkuAndDelivery(STORE_ID, ITEM_SKU);
  }
}
