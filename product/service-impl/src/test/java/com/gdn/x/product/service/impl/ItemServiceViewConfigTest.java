package com.gdn.x.product.service.impl;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;
import static org.mockito.MockitoAnnotations.openMocks;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;

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

import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.x.product.enums.ChannelName;
import com.gdn.x.product.model.entity.Item;
import com.gdn.x.product.model.entity.ItemPickupPoint;
import com.gdn.x.product.model.entity.ItemViewConfig;
import com.gdn.x.product.model.entity.MasterDataItem;
import com.gdn.x.product.model.entity.Price;
import com.gdn.x.product.model.entity.Product;
import com.gdn.x.product.model.vo.ItemVo;
import com.gdn.x.product.service.api.CacheEvictHelperService;
import com.gdn.x.product.service.api.ChannelService;
import com.gdn.x.product.service.api.ItemPickupPointService;
import com.gdn.x.product.service.api.ItemService;
import com.gdn.x.product.service.api.ObjectConverterService;
import com.gdn.x.product.service.api.ProductAndItemSolrIndexerService;
import com.gdn.x.product.service.api.ProductHelperService;
import com.gdn.x.product.service.api.ProductService;
import com.gdn.x.product.service.api.SaveAndPublishService;
import com.gdn.x.product.service.api.SaveOperationService;
import com.gdn.x.product.service.api.SkuValidator;

public class ItemServiceViewConfigTest {

  private static final String STORE_ID = "store-id";

  private static final String REQUEST_ID = "request-id";

  private static final String ITEM_SKU = "item-sku";

  private static final String PRODUCT_SKU = "product-sku";

  private static final String ITEM_CODE = "item-code";

  private static final boolean IS_SYNCHRONIZED = true;

  private static final String CHANNEL_DEFAULT = ChannelName.DEFAULT.toString();

  private static final String CHANNEL_WEB = ChannelName.DESKTOP_WEB.toString();

  private static final String ITEM_SKU_NOT_FOUND = "item-sku-not-found";

  private static final String CHANNEL_MOBILE = ChannelName.MOBILE_WEB.toString();

  private static final String USERNAME = "username";

  private static final int STOCK = 2;

  private static final int ORIGINAL_STOCK = 4;

  private static final int RESERVE_STOCK = ItemServiceViewConfigTest.ORIGINAL_STOCK
      - ItemServiceViewConfigTest.STOCK;

  private static final String MERCHANT_SKU = "merchant-sku";

  private static final String MERCHANT_CODE = "level2-merchant-code";

  private static final boolean BUYABLE = true;

  private static final String MERCHANT_CODE_NOT_FOUND = "merchant-code-not-found";

  @InjectMocks
  private ItemViewConfigServiceImpl itemServiceImpl;

  @Mock
  private ObjectConverterService objectConverterService;

  @Mock
  private ProductService productService;

  @Mock
  private ItemService itemRepository;

  @Mock
  private ProductHelperService productHelperService;

  @Mock
  private SaveOperationService saveOperationService;

  @Mock
  private ChannelService channelService;

  @Mock
  private SkuValidator skuValidator;

  @Mock
  private SaveAndPublishService saveAndPublishService;

  @Mock
  private ProductAndItemSolrIndexerService productAndItemSolrIndexerService;

  @Mock
  private ItemPickupPointService itemPickupPointService;

  @Mock
  private CacheEvictHelperService cacheEvictHelperService;

  private Item item;

  private MasterDataItem masterDataItem;

  private HashSet<Price> prices;

  private ItemViewConfig itemViewConfigDefault;

  private ItemViewConfig itemViewConfigMobile;

  private ItemViewConfig itemViewConfigWeb;

  private Item itemWithEmptyPrice;

  private List<Item> listOfItems;

  private Product product;

  private List<Product> setOfProducts;

  @Captor
  private ArgumentCaptor<List<Item>> argumentCaptorListOfItems;

  private ItemViewConfig itemViewConfigUpdated;

  private Item itemWithViewConfigs;

  private HashSet<ItemViewConfig> itemViewConfigSet;

  private ItemViewConfig itemViewConfigUpdatedWithChannelNotFound;

  private List<Product> emptySetOfProducts;

  @Test
  public void addItemViewConfigByMerchantSkuAndMerchantCodeTest() {
    boolean result =
        this.itemServiceImpl.addItemViewConfigByMerchantSkuAndMerchantCode(
            ItemServiceViewConfigTest.STORE_ID, ItemServiceViewConfigTest.REQUEST_ID,
            ItemServiceViewConfigTest.USERNAME, ItemServiceViewConfigTest.MERCHANT_SKU,
            ItemServiceViewConfigTest.MERCHANT_CODE, this.itemViewConfigMobile);

    verify(this.itemRepository).getItemsByMerchantSkuAndMerchantCode(
        ItemServiceViewConfigTest.STORE_ID, ItemServiceViewConfigTest.MERCHANT_SKU,
        ItemServiceViewConfigTest.MERCHANT_CODE);
    verify(this.saveOperationService).saveItemsAndClearCacheWithoutUpdatingSolr(this.argumentCaptorListOfItems.capture(),
        Mockito.any(), anyString());
    verify(this.productAndItemSolrIndexerService).updateSolrOnItemViewConfigChanges(this.argumentCaptorListOfItems.capture());

    assertTrue(result);
  }

  @Test
  public void addItemViewConfigByMerchantSkuAndMerchantCodeTestWithChannelExist() {
    try {
      this.itemServiceImpl.addItemViewConfigByMerchantSkuAndMerchantCode(
          ItemServiceViewConfigTest.STORE_ID, ItemServiceViewConfigTest.REQUEST_ID,
          ItemServiceViewConfigTest.USERNAME, ItemServiceViewConfigTest.MERCHANT_SKU,
          ItemServiceViewConfigTest.MERCHANT_CODE, this.itemViewConfigDefault);
    } catch (Exception e) {
      verify(this.itemRepository).getItemsByMerchantSkuAndMerchantCode(
          ItemServiceViewConfigTest.STORE_ID, ItemServiceViewConfigTest.MERCHANT_SKU,
          ItemServiceViewConfigTest.MERCHANT_CODE);
      assertTrue(e instanceof ApplicationRuntimeException);
    }
  }

  @Test
  public void addItemViewConfigByMerchantSkuAndMerchantCodeTestWithIMerchantCodeNotFound() {
    boolean result =
        this.itemServiceImpl.addItemViewConfigByMerchantSkuAndMerchantCode(
            ItemServiceViewConfigTest.STORE_ID, ItemServiceViewConfigTest.REQUEST_ID,
            ItemServiceViewConfigTest.USERNAME, ItemServiceViewConfigTest.MERCHANT_SKU,
            ItemServiceViewConfigTest.MERCHANT_CODE_NOT_FOUND, this.itemViewConfigMobile);

    verify(this.itemRepository).getItemsByMerchantSkuAndMerchantCode(
        ItemServiceViewConfigTest.STORE_ID, ItemServiceViewConfigTest.MERCHANT_SKU,
        ItemServiceViewConfigTest.MERCHANT_CODE_NOT_FOUND);
    verify(this.saveOperationService).saveItemsAndClearCacheWithoutUpdatingSolr(this.argumentCaptorListOfItems.capture(),
        Mockito.any(), anyString());
    verify(this.productAndItemSolrIndexerService).updateSolrOnItemViewConfigChanges(this.argumentCaptorListOfItems.capture());
    assertTrue(result);
  }

  @Test
  public void addItemViewConfigByMerchantSkuAndMerchantCodeTestWithMerchantCodeBlank() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () ->  this.itemServiceImpl.addItemViewConfigByMerchantSkuAndMerchantCode(
        ItemServiceViewConfigTest.STORE_ID, ItemServiceViewConfigTest.REQUEST_ID,
        ItemServiceViewConfigTest.USERNAME, ItemServiceViewConfigTest.MERCHANT_SKU, null,
        this.itemViewConfigUpdated));
  }

  @Test
  public void addItemViewConfigByMerchantSkuAndMerchantCodeTestWithNullItemViewConfig() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () ->  this.itemServiceImpl.addItemViewConfigByMerchantSkuAndMerchantCode(
        ItemServiceViewConfigTest.STORE_ID, ItemServiceViewConfigTest.REQUEST_ID,
        ItemServiceViewConfigTest.USERNAME, ItemServiceViewConfigTest.MERCHANT_SKU,
        ItemServiceViewConfigTest.MERCHANT_CODE, null));
  }

  @Test
  public void addItemViewConfigByMerchantSkuAndMerchantCodeTestWithStoreIdBlank() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () ->  this.itemServiceImpl.addItemViewConfigByMerchantSkuAndMerchantCode(null,
        ItemServiceViewConfigTest.REQUEST_ID, ItemServiceViewConfigTest.USERNAME,
        ItemServiceViewConfigTest.MERCHANT_SKU, ItemServiceViewConfigTest.MERCHANT_CODE,
        this.itemViewConfigUpdated));
  }

  @Test
  public void addItemViewConfigTest() throws Exception {
    boolean result =
        this.itemServiceImpl.addItemViewConfig(ItemServiceViewConfigTest.STORE_ID,
            ItemServiceViewConfigTest.ITEM_SKU, this.itemViewConfigMobile);
    verify(this.itemRepository).findByStoreIdAndItemSkuAndMarkForDeleteFalse(
        ItemServiceViewConfigTest.STORE_ID, ItemServiceViewConfigTest.ITEM_SKU);
    verify(this.saveOperationService).saveItemWithoutUpdatingSolr(this.item, null, false, StringUtils.EMPTY, Collections.EMPTY_MAP);
    verify(this.productAndItemSolrIndexerService).updateSolrOnItemViewConfigChanges(Arrays.asList(item));
    assertTrue(result);
  }

  @Test
  public void addItemViewConfigTestWithBlankItemSku() throws Exception {
    Assertions.assertThrows(ApplicationRuntimeException.class, () ->  this.itemServiceImpl.addItemViewConfig(ItemServiceViewConfigTest.STORE_ID, null,
        this.itemViewConfigMobile));
  }

  @Test
  public void addItemViewConfigTestWithBlankStoreId() throws Exception {
    Assertions.assertThrows(ApplicationRuntimeException.class, () ->  this.itemServiceImpl.addItemViewConfig(null, ItemServiceViewConfigTest.ITEM_SKU,
        this.itemViewConfigMobile));
  }

  @Test
  public void addItemViewConfigTestWithChannelExist() throws Exception {
    try {
      this.itemServiceImpl.addItemViewConfig(ItemServiceViewConfigTest.STORE_ID,
          ItemServiceViewConfigTest.ITEM_SKU, this.itemViewConfigDefault);
    } catch (Exception e) {
      verify(this.itemRepository).findByStoreIdAndItemSkuAndMarkForDeleteFalse(
          ItemServiceViewConfigTest.STORE_ID, ItemServiceViewConfigTest.ITEM_SKU);
      assertTrue(e instanceof ApplicationRuntimeException);
    }
  }

  @Test
  public void addItemViewConfigTestWithNoItemFound() throws Exception {
    try {
      this.itemServiceImpl.addItemViewConfig(ItemServiceViewConfigTest.STORE_ID,
          ItemServiceViewConfigTest.ITEM_SKU_NOT_FOUND, this.itemViewConfigDefault);
    } catch (Exception e) {
      verify(this.itemRepository).findByStoreIdAndItemSkuAndMarkForDeleteFalse(
          ItemServiceViewConfigTest.STORE_ID, ItemServiceViewConfigTest.ITEM_SKU_NOT_FOUND);
      assertTrue(e instanceof ApplicationRuntimeException);
    }
  }

  @Test
  public void addItemViewConfigTestWithNullItemViewConfig() throws Exception {
    Assertions.assertThrows(ApplicationRuntimeException.class, () ->  this.itemServiceImpl.addItemViewConfig(ItemServiceViewConfigTest.STORE_ID,
        ItemServiceViewConfigTest.ITEM_SKU, null));
  }

  @Test
  public void deleteItemViewConfigTest() throws Exception {
    boolean result =
        this.itemServiceImpl.deleteItemViewConfig(ItemServiceViewConfigTest.STORE_ID,
            ItemServiceViewConfigTest.ITEM_SKU, ItemServiceViewConfigTest.CHANNEL_WEB);
    verify(this.itemRepository).findByStoreIdAndItemSkuAndMarkForDeleteFalse(
        ItemServiceViewConfigTest.STORE_ID, ItemServiceViewConfigTest.ITEM_SKU);
    verify(this.saveOperationService).saveItemWithoutUpdatingSolr(this.item, null, false, StringUtils.EMPTY, Collections.EMPTY_MAP);
    verify(this.channelService).getDefaultChannel();
    verify(this.skuValidator).isItemSku(ItemServiceViewConfigTest.ITEM_SKU);
    verify(this.productAndItemSolrIndexerService).updateSolrOnItemViewConfigChanges(Arrays.asList(item));
    assertEquals(result, true);
  }

  @Test
  public void deleteItemViewConfigTestWithBlankStoreId() throws Exception {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.itemServiceImpl.deleteItemViewConfig(null, ItemServiceViewConfigTest.ITEM_SKU,
        ItemServiceViewConfigTest.CHANNEL_WEB));
  }

  @Test
  public void deleteItemViewConfigTestWithDefaultChannel() throws Exception {
    try {
      this.itemServiceImpl.deleteItemViewConfig(ItemServiceViewConfigTest.STORE_ID,
          ItemServiceViewConfigTest.ITEM_SKU, ItemServiceViewConfigTest.CHANNEL_DEFAULT);
    } catch (Exception e) {
      verify(this.channelService).getDefaultChannel();
      verify(this.skuValidator).isItemSku(ItemServiceViewConfigTest.ITEM_SKU);
    }
  }

  @Test
  public void deleteItemViewConfigTestWithNoChannelFound() throws Exception {
    try {
      this.itemServiceImpl.deleteItemViewConfig(ItemServiceViewConfigTest.STORE_ID,
          ItemServiceViewConfigTest.ITEM_SKU, ItemServiceViewConfigTest.CHANNEL_MOBILE);
    } catch (Exception e) {
      verify(this.itemRepository).findByStoreIdAndItemSkuAndMarkForDeleteFalse(
          ItemServiceViewConfigTest.STORE_ID, ItemServiceViewConfigTest.ITEM_SKU);
      verify(this.channelService).getDefaultChannel();
      verify(this.skuValidator).isItemSku(ItemServiceViewConfigTest.ITEM_SKU);
    }
  }

  @Test
  public void deleteItemViewConfigTestWithNoItemFound() throws Exception {
    try {
      this.itemServiceImpl.deleteItemViewConfig(ItemServiceViewConfigTest.STORE_ID,
          ItemServiceViewConfigTest.ITEM_SKU_NOT_FOUND, ItemServiceViewConfigTest.CHANNEL_WEB);
    } catch (Exception e) {
      verify(this.itemRepository).findByStoreIdAndItemSkuAndMarkForDeleteFalse(
          ItemServiceViewConfigTest.STORE_ID, ItemServiceViewConfigTest.ITEM_SKU_NOT_FOUND);
      verify(this.channelService).getDefaultChannel();
      verify(this.skuValidator).isItemSku(ItemServiceViewConfigTest.ITEM_SKU_NOT_FOUND);
    }
  }

  @Test
  public void deleteItemViewConfigTestWithNotValidItemSkuFormat() throws Exception {
    when(this.skuValidator.isItemSku(ItemServiceViewConfigTest.ITEM_SKU)).thenReturn(false);
    try {
      this.itemServiceImpl.deleteItemViewConfig(ItemServiceViewConfigTest.STORE_ID,
          ItemServiceViewConfigTest.ITEM_SKU, ItemServiceViewConfigTest.CHANNEL_WEB);
    } catch (Exception e) {
      verify(this.skuValidator).isItemSku(ItemServiceViewConfigTest.ITEM_SKU);
    }
  }

  @Test
  public void deleteItemViewConfigTestWithNullChannel() throws Exception {
    try {
      this.itemServiceImpl.deleteItemViewConfig(ItemServiceViewConfigTest.STORE_ID,
          ItemServiceViewConfigTest.ITEM_SKU, null);
    } catch (Exception e) {
      verify(this.skuValidator).isItemSku(ItemServiceViewConfigTest.ITEM_SKU);
    }
  }

  @BeforeEach
  public void setUp() throws Exception {
    openMocks(this);
    when(this.skuValidator.isItemSku(ItemServiceViewConfigTest.ITEM_SKU)).thenReturn(true);
    when(this.skuValidator.isItemSku(ItemServiceViewConfigTest.ITEM_SKU_NOT_FOUND))
        .thenReturn(true);
    when(this.channelService.getDefaultChannel()).thenReturn(ChannelName.DEFAULT.toString());

    this.itemViewConfigWeb = new ItemViewConfig();
    this.itemViewConfigWeb.setChannel(ItemServiceViewConfigTest.CHANNEL_WEB);

    this.itemViewConfigDefault = new ItemViewConfig();
    this.itemViewConfigDefault.setChannel(ItemServiceViewConfigTest.CHANNEL_DEFAULT);

    this.itemViewConfigMobile = new ItemViewConfig();
    this.itemViewConfigMobile.setChannel(ItemServiceViewConfigTest.CHANNEL_MOBILE);

    this.item = new Item();
    this.item.setItemSku(ItemServiceViewConfigTest.ITEM_SKU);
    this.item.setProductSku(ItemServiceViewConfigTest.PRODUCT_SKU);
    this.item.setItemCode(ItemServiceViewConfigTest.ITEM_CODE);
    this.item.setSynchronized(ItemServiceViewConfigTest.IS_SYNCHRONIZED);
    this.item.setPrice(this.prices);
    this.item.setMasterDataItem(this.masterDataItem);
    this.item.getItemViewConfigs().add(this.itemViewConfigDefault);
    this.item.getItemViewConfigs().add(this.itemViewConfigWeb);

    when(
        this.itemRepository.findByStoreIdAndItemSkuAndMarkForDeleteFalse(
            ItemServiceViewConfigTest.STORE_ID, ItemServiceViewConfigTest.ITEM_SKU)).thenReturn(
        this.item);

    when(this.saveOperationService.insertItem(this.item)).thenReturn(this.item);

    when(
        this.itemRepository.findByStoreIdAndItemSkuAndMarkForDeleteFalse(
            ItemServiceViewConfigTest.STORE_ID, ItemServiceViewConfigTest.ITEM_SKU_NOT_FOUND))
        .thenReturn(null);

    when(
        this.productHelperService.updateItemViewConfigForExistingChannel(this.item,
            this.itemViewConfigDefault)).thenReturn(this.item);

    this.itemWithEmptyPrice = new Item();
    this.itemWithEmptyPrice.setProductSku(ItemServiceViewConfigTest.PRODUCT_SKU);

    this.itemViewConfigUpdated = new ItemViewConfig();
    this.itemViewConfigUpdated.setChannel(ItemServiceViewConfigTest.CHANNEL_WEB);
    this.itemViewConfigUpdated.setBuyable(ItemServiceViewConfigTest.BUYABLE);

    this.itemViewConfigSet = new HashSet<ItemViewConfig>();
    this.itemViewConfigSet.add(this.itemViewConfigUpdated);

    this.itemWithViewConfigs = new Item();
    this.itemWithViewConfigs.setItemViewConfigs(this.itemViewConfigSet);

    this.listOfItems = new ArrayList<Item>();
    this.listOfItems.add(this.item);
    this.listOfItems.add(this.itemWithViewConfigs);
    this.listOfItems.add(this.itemWithEmptyPrice);

    when(
        this.itemRepository.getItemsByMerchantSkuAndMerchantCode(
            ItemServiceViewConfigTest.STORE_ID, ItemServiceViewConfigTest.MERCHANT_SKU,
            ItemServiceViewConfigTest.MERCHANT_CODE)).thenReturn(this.listOfItems);

    this.product = new Product();
    this.product.setProductSku(ItemServiceViewConfigTest.PRODUCT_SKU);

    this.setOfProducts = new ArrayList<>();
    this.setOfProducts.add(this.product);

    when(this.saveOperationService.saveItem(this.item, null, new ArrayList<>())).thenReturn(this.item);

    when(
        this.productService.getProductsByMerchantCode(ItemServiceViewConfigTest.STORE_ID,
            ItemServiceViewConfigTest.MERCHANT_CODE)).thenReturn(this.setOfProducts);

    this.emptySetOfProducts = new ArrayList<>();
    when(
        this.productService.getProductsByMerchantCode(ItemServiceViewConfigTest.STORE_ID,
            ItemServiceViewConfigTest.MERCHANT_CODE_NOT_FOUND)).thenReturn(this.emptySetOfProducts);

    when(this.saveOperationService.saveItems(this.argumentCaptorListOfItems.capture(), Mockito.any()))
        .thenReturn(this.listOfItems);

    this.itemViewConfigUpdatedWithChannelNotFound = new ItemViewConfig();
    this.itemViewConfigUpdatedWithChannelNotFound
        .setChannel(ItemServiceViewConfigTest.CHANNEL_MOBILE);
  }

  @AfterEach
  public void tearDown() {
    verifyNoMoreInteractions(this.objectConverterService);
    verifyNoMoreInteractions(this.itemRepository);
    verifyNoMoreInteractions(this.productHelperService);
    verifyNoMoreInteractions(this.productService);
    verifyNoMoreInteractions(this.saveOperationService);
    verifyNoMoreInteractions(this.channelService);
    verifyNoMoreInteractions(this.skuValidator);
    verifyNoMoreInteractions(this.saveAndPublishService);
  }

  @Test
  public void updateItemViewConfigTest() throws Exception {
    when(productHelperService.updateItemViewConfigForExistingChannelItemPickupPoint(any(),any())).thenReturn(true);
    when(itemPickupPointService.findByItemSkuAndDelivery(STORE_ID, ITEM_SKU)).thenReturn(
        ItemPickupPoint.builder().itemSku(item.getItemSku()).pickupPointCode(item.getPickupPointCode())
            .itemViewConfig(item.getItemViewConfigs()).price(item.getPrice()).build());
    when(itemPickupPointService.saveItemPickupPoint(Mockito.any(ItemPickupPoint.class))).thenReturn(new ItemPickupPoint());
    doNothing().when(cacheEvictHelperService)
        .evictItemPickupPointData(Mockito.anyString(), Mockito.any(ItemPickupPoint.class), Mockito.anyString());
    boolean result = this.itemServiceImpl.updateItemViewConfig(ItemServiceViewConfigTest.STORE_ID,
        ItemServiceViewConfigTest.ITEM_SKU, this.itemViewConfigDefault);
    verify(itemRepository).findByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, ITEM_SKU);
    verify(productHelperService).updateItemViewConfigForExistingChannelItemPickupPoint(
        Mockito.any(ItemPickupPoint.class), Mockito.any(ItemViewConfig.class));
    verify(productAndItemSolrIndexerService).updateSolrOnItemViewConfigChangesByItemPickupPoint(
        Mockito.anyList());
    verify(saveAndPublishService).publishMerchantVoucherViewConfigChange(Mockito.any(ItemVo.class));
    verify(saveAndPublishService).publishItemPickupPointDataChangeEvent(Mockito.any(), Mockito.any(),
      eq(Collections.EMPTY_MAP));
    verify(itemPickupPointService).findByItemSkuAndDelivery(STORE_ID, ITEM_SKU);
    verify(itemPickupPointService).saveItemPickupPoint(Mockito.any(ItemPickupPoint.class));
    verify(cacheEvictHelperService).evictItemPickupPointData(Mockito.isNull(), Mockito.any(ItemPickupPoint.class), Mockito.isNull());
    verify(cacheEvictHelperService).evictItemData(Mockito.isNull(), Mockito.any(Item.class));
    assertTrue(result);
  }

  @Test
  public void updateItemViewForceReviewConfig() {
    item.setForceReview(true);
    when(this.itemRepository.findByStoreIdAndItemSkuAndMarkForDeleteFalse(ItemServiceViewConfigTest.STORE_ID,
        ItemServiceViewConfigTest.ITEM_SKU)).thenReturn(this.item);
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () ->  this.itemServiceImpl
          .updateItemViewConfig(ItemServiceViewConfigTest.STORE_ID, ItemServiceViewConfigTest.ITEM_SKU,
              this.itemViewConfigDefault));
    } finally {
      verify(this.itemRepository).findByStoreIdAndItemSkuAndMarkForDeleteFalse(ItemServiceViewConfigTest.STORE_ID,
          ItemServiceViewConfigTest.ITEM_SKU);
    }
  }

  @Test
  public void updateItemViewConfigByMerchantSkuAndMerchantCodeTest() {
    this.listOfItems.remove(this.itemWithEmptyPrice);
    this.listOfItems.remove(this.itemWithViewConfigs);
    boolean result =
        this.itemServiceImpl.updateItemViewConfigByMerchantSkuAndMerchantCode(
            ItemServiceViewConfigTest.STORE_ID, ItemServiceViewConfigTest.REQUEST_ID,
            ItemServiceViewConfigTest.USERNAME, ItemServiceViewConfigTest.MERCHANT_SKU,
            ItemServiceViewConfigTest.MERCHANT_CODE, this.itemViewConfigUpdated);

    verify(this.itemRepository).getItemsByMerchantSkuAndMerchantCode(
        ItemServiceViewConfigTest.STORE_ID, ItemServiceViewConfigTest.MERCHANT_SKU,
        ItemServiceViewConfigTest.MERCHANT_CODE);
    verify(this.saveOperationService).saveItemsAndClearCacheWithoutUpdatingSolr(this.listOfItems, null,
        StringUtils.EMPTY);
    verify(this.productAndItemSolrIndexerService).updateSolrOnItemViewConfigChanges(this.listOfItems);
    verify(saveAndPublishService).publishMerchantVoucherViewConfigChange(Mockito.anyList(), Mockito.anyList());
    assertTrue(result);
  }

  @Test
  public void updateItemViewConfigByMerchantSkuAndMerchantCodeTestWithChannelNotFound() {
    this.itemServiceImpl.updateItemViewConfigByMerchantSkuAndMerchantCode(
        ItemServiceViewConfigTest.STORE_ID, ItemServiceViewConfigTest.REQUEST_ID,
        ItemServiceViewConfigTest.USERNAME, ItemServiceViewConfigTest.MERCHANT_SKU,
        ItemServiceViewConfigTest.MERCHANT_CODE, this.itemViewConfigUpdatedWithChannelNotFound);
    verify(this.itemRepository).getItemsByMerchantSkuAndMerchantCode(
        ItemServiceViewConfigTest.STORE_ID, ItemServiceViewConfigTest.MERCHANT_SKU,
        ItemServiceViewConfigTest.MERCHANT_CODE);
    verify(this.saveOperationService).saveItemsAndClearCacheWithoutUpdatingSolr(this.argumentCaptorListOfItems.capture(),
        Mockito.any(), anyString());
    verify(this.productAndItemSolrIndexerService).updateSolrOnItemViewConfigChanges(this.argumentCaptorListOfItems.capture());
    verify(saveAndPublishService)
        .publishMerchantVoucherViewConfigChange(Mockito.anyList(), Mockito.anyList());
  }

  @Test
  public void updateItemViewConfigByMerchantSkuAndMerchantCodeTestWithNullItemViewConfig() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () ->  this.itemServiceImpl.updateItemViewConfigByMerchantSkuAndMerchantCode(
        ItemServiceViewConfigTest.STORE_ID, ItemServiceViewConfigTest.REQUEST_ID,
        ItemServiceViewConfigTest.USERNAME, ItemServiceViewConfigTest.MERCHANT_SKU,
        ItemServiceViewConfigTest.MERCHANT_CODE, null));
  }

  @Test
  public void updateItemViewConfigByMerchantSkuAndMerchantCodeTestWithNullMerchantCode() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () ->  this.itemServiceImpl.updateItemViewConfigByMerchantSkuAndMerchantCode(
        ItemServiceViewConfigTest.STORE_ID, ItemServiceViewConfigTest.REQUEST_ID,
        ItemServiceViewConfigTest.USERNAME, ItemServiceViewConfigTest.MERCHANT_SKU, null,
        this.itemViewConfigUpdated));
  }

  @Test
  public void updateItemViewConfigByMerchantSkuAndMerchantCodeTestWithNullStoreId() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () ->  this.itemServiceImpl.updateItemViewConfigByMerchantSkuAndMerchantCode(null,
        ItemServiceViewConfigTest.REQUEST_ID, ItemServiceViewConfigTest.USERNAME,
        ItemServiceViewConfigTest.MERCHANT_SKU, ItemServiceViewConfigTest.MERCHANT_CODE,
        this.itemViewConfigUpdated));
  }

  @Test
  public void updateItemViewConfigWithBlankItemSku() throws Exception {
    Assertions.assertThrows(ApplicationRuntimeException.class, () ->  this.itemServiceImpl.updateItemViewConfig(ItemServiceViewConfigTest.STORE_ID, null,
        this.itemViewConfigDefault));
  }

  @Test
  public void updateItemViewConfigWithBlankStoreId() throws Exception {
    Assertions.assertThrows(ApplicationRuntimeException.class, () ->  this.itemServiceImpl.updateItemViewConfig(null, ItemServiceViewConfigTest.ITEM_SKU,
        this.itemViewConfigDefault));
  }

  @Test
  public void updateItemViewConfigWithNoItemFound() throws Exception {
    try {
      this.itemServiceImpl.updateItemViewConfig(ItemServiceViewConfigTest.STORE_ID,
          ItemServiceViewConfigTest.ITEM_SKU_NOT_FOUND, this.itemViewConfigDefault);
    } catch (Exception e) {
      verify(this.itemRepository).findByStoreIdAndItemSkuAndMarkForDeleteFalse(
          ItemServiceViewConfigTest.STORE_ID, ItemServiceViewConfigTest.ITEM_SKU_NOT_FOUND);
    }
  }

  @Test
  public void updateItemViewConfigWithNullChannel() throws Exception {
    Assertions.assertThrows(ApplicationRuntimeException.class, () ->  this.itemServiceImpl.updateItemViewConfig(ItemServiceViewConfigTest.STORE_ID,
        ItemServiceViewConfigTest.ITEM_SKU, null));
  }
}
