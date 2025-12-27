package com.gdn.x.product.service.impl;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.anyBoolean;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;
import static org.mockito.MockitoAnnotations.openMocks;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Date;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

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
import com.gdn.x.product.dao.api.ItemRepository;
import com.gdn.x.product.domain.event.model.ItemPickupPointDataChangeEventModel;
import com.gdn.x.product.enums.ChannelName;
import com.gdn.x.product.enums.Constants;
import com.gdn.x.product.enums.ItemChangeEventType;
import com.gdn.x.product.model.entity.DiscountPrice;
import com.gdn.x.product.model.entity.Item;
import com.gdn.x.product.model.entity.ItemPickupPoint;
import com.gdn.x.product.model.entity.Price;
import com.gdn.x.product.model.entity.PriceHistory;
import com.gdn.x.product.model.entity.Product;
import com.gdn.x.product.service.api.CacheEvictHelperService;
import com.gdn.x.product.service.api.ChannelService;
import com.gdn.x.product.service.api.ItemHelperService;
import com.gdn.x.product.service.api.ItemPickupPointService;
import com.gdn.x.product.service.api.ObjectConverterService;
import com.gdn.x.product.service.api.PriceHistoryService;
import com.gdn.x.product.service.api.ProductAndItemSolrIndexerService;
import com.gdn.x.product.service.api.ProductService;
import com.gdn.x.product.service.api.SaveAndPublishService;
import com.gdn.x.product.service.api.SaveOperationService;
import com.gdn.x.product.service.api.SkuValidator;
import com.google.common.collect.Sets;

public class ItemServicePriceTest {

  private static final String STORE_ID = "store-id";

  private static final Date START_DATE_TIME = new Date();

  private static final Date END_DATE_TIME = new Date();

  private static final double SALE_PRICE = 1000;

  private static final double LIST_PRICE = 5000;

  private static final String CURRENCY = "currency";

  private static final double OFFER_PRICE = 4000;

  private static final String ITEM_SKU = "item-sku";

  private static final String PRODUCT_SKU = "product-sku";

  private static final String ITEM_CODE = "item-code";

  private static final boolean IS_SYNCHRONIZED = true;

  private static final String CHANNEL_DEFAULT = ChannelName.DEFAULT.toString();

  private static final String CHANNEL_WEB = ChannelName.DESKTOP_WEB.toString();

  private static final String ITEM_SKU_NOT_FOUND = "item-sku-not-found";

  private static final String ITEM_SKU_FOR_PRICE_WITH_CHANNEL_NOT_FOUND =
      "item-sku-for-priceDefault-not-found";

  private static final String CHANNEL_MOBILE = ChannelName.MOBILE_WEB.toString();

  private static final String ITEM_SKU_WITH_EMPTY_PRICE = "item-sku-with-empty-priceDefault";

  private static final String REQUEST_ID = "request-id";

  private static final String USERNAME = "username";

  private static final String MERCHANT_SKU = "merchant-sku";

  private static final String MERCHANT_CODE = "merchant-code";

  private static final String MERCHANT_CODE_NOT_FOUND = "merchant-code-not-found";

  @InjectMocks
  private ItemServiceImpl itemServiceImpl;

  @Mock
  private ObjectConverterService objectConverterService;

  @Mock
  private ItemRepository itemRepository;

  @Mock
  private PriceHistoryService priceHistoryService;

  @Mock
  private ProductService productService;

  @Mock
  private SaveOperationService saveOperationService;

  @Mock
  private ChannelService channelService;

  @Mock
  private SkuValidator skuValidator;

  @Mock
  private ItemHelperService itemHelperService;

  @Mock
  private ProductAndItemSolrIndexerService productAndItemSolrIndexerService;

  @Mock
  private ItemPickupPointService itemPickupPointService;

  @Mock
  private CacheEvictHelperService cacheEvictHelperService;

  @Mock
  private SaveAndPublishService saveAndPublishService;

  private Item item;

  private Price priceDefault;

  private DiscountPrice discountPrice;

  private HashSet<Price> prices;

  private Price priceWeb;

  private PriceHistory priceHistory;

  private Price newPriceWeb;

  private List<DiscountPrice> discountPriceList;

  private Price itemPriceWithChannelNotFound;

  private Item itemWithEmptyPrice;

  private List<Item> listOfItems;

  private List<Product> setOfProducts;

  private Product product;

  private Item itemWithDifferentProductSku;

  @Captor
  private ArgumentCaptor<List<Item>> argumentCaptorListOfItems;

  @Captor
  private ArgumentCaptor<Item> itemArgumentCaptor;

  private List<Product> emptySetOfProducts;

  @Test
  public void addItemPriceByMerchantSkuAndMerchantCodeTest() {
    this.prices.remove(this.priceWeb);
    boolean result =
        this.itemServiceImpl.addItemPriceByMerchantSkuAndMerchantCode(
            ItemServicePriceTest.STORE_ID, ItemServicePriceTest.REQUEST_ID,
            ItemServicePriceTest.USERNAME, ItemServicePriceTest.MERCHANT_SKU,
            ItemServicePriceTest.MERCHANT_CODE, this.priceWeb);
    verify(this.itemRepository).findItemsByStoreIdAndMerchantSkuAndMarkForDeleteFalse(
        ItemServicePriceTest.STORE_ID, ItemServicePriceTest.MERCHANT_SKU);
    verify(this.productService).getProductsByMerchantCode(ItemServicePriceTest.STORE_ID,
        ItemServicePriceTest.MERCHANT_CODE);
    verify(this.saveOperationService).saveItems(this.argumentCaptorListOfItems.capture(), Mockito.any());

    assertTrue(result);
  }

  @Test
  public void addItemPriceByMerchantSkuAndMerchantCodeEventTypeTest() {
    this.prices.remove(this.priceWeb);
    boolean result =
        this.itemServiceImpl.addItemPriceByMerchantSkuAndMerchantCode(
            ItemServicePriceTest.STORE_ID, ItemServicePriceTest.REQUEST_ID,
            ItemServicePriceTest.USERNAME, ItemServicePriceTest.MERCHANT_SKU,
            ItemServicePriceTest.MERCHANT_CODE, this.priceWeb);
    verify(this.itemRepository).findItemsByStoreIdAndMerchantSkuAndMarkForDeleteFalse(
        ItemServicePriceTest.STORE_ID, ItemServicePriceTest.MERCHANT_SKU);
    verify(this.productService).getProductsByMerchantCode(ItemServicePriceTest.STORE_ID,
        ItemServicePriceTest.MERCHANT_CODE);
    verify(this.saveOperationService).saveItems(this.argumentCaptorListOfItems.capture(), Mockito.any());
    Assertions.assertEquals(1, argumentCaptorListOfItems.getValue().get(0).getItemChangeEventTypes().size());
    Assertions.assertEquals(ItemChangeEventType.ITEM_PRICE_CHANGE, argumentCaptorListOfItems.getValue().get(0).getItemChangeEventTypes().get(0));
    assertTrue(result);
  }

  @Test
  public void addItemPriceByMerchantSkuAndMerchantCodeTestWithChannelExist() {
    try {
      this.itemServiceImpl.addItemPriceByMerchantSkuAndMerchantCode(ItemServicePriceTest.STORE_ID,
          ItemServicePriceTest.REQUEST_ID, ItemServicePriceTest.USERNAME,
          ItemServicePriceTest.MERCHANT_SKU, ItemServicePriceTest.MERCHANT_CODE, this.priceWeb);
    } catch (Exception e) {
      verify(this.itemRepository).findItemsByStoreIdAndMerchantSkuAndMarkForDeleteFalse(
          ItemServicePriceTest.STORE_ID, ItemServicePriceTest.MERCHANT_SKU);
      verify(this.productService).getProductsByMerchantCode(ItemServicePriceTest.STORE_ID,
          ItemServicePriceTest.MERCHANT_CODE);
      assertTrue(e instanceof ApplicationRuntimeException);
    }
  }

  @Test
  public void addItemPriceByMerchantSkuAndMerchantCodeTestWithMerchantCodeBlank() {
    this.prices.remove(this.priceWeb);
    Assertions.assertThrows(ApplicationRuntimeException.class, () ->  this.itemServiceImpl.addItemPriceByMerchantSkuAndMerchantCode(ItemServicePriceTest.STORE_ID,
        ItemServicePriceTest.REQUEST_ID, ItemServicePriceTest.USERNAME,
        ItemServicePriceTest.MERCHANT_SKU, null, this.priceWeb));
  }

  @Test
  public void addItemPriceByMerchantSkuAndMerchantCodeTestWithMerchantCodeNotFound() {
    this.prices.remove(this.priceWeb);
    try {
      this.itemServiceImpl.addItemPriceByMerchantSkuAndMerchantCode(ItemServicePriceTest.STORE_ID,
          ItemServicePriceTest.REQUEST_ID, ItemServicePriceTest.USERNAME,
          ItemServicePriceTest.MERCHANT_SKU, ItemServicePriceTest.MERCHANT_CODE_NOT_FOUND,
          this.priceWeb);
    } catch (Exception e) {
      verify(this.itemRepository).findItemsByStoreIdAndMerchantSkuAndMarkForDeleteFalse(
          ItemServicePriceTest.STORE_ID, ItemServicePriceTest.MERCHANT_SKU);
      verify(this.productService).getProductsByMerchantCode(ItemServicePriceTest.STORE_ID,
          ItemServicePriceTest.MERCHANT_CODE_NOT_FOUND);
      assertTrue(e instanceof ApplicationRuntimeException);
    }
  }

  @Test
  public void addItemPriceByMerchantSkuAndMerchantCodeTestWithNullPrice() {
    this.prices.remove(this.priceWeb);
    Assertions.assertThrows(ApplicationRuntimeException.class, () ->  this.itemServiceImpl.addItemPriceByMerchantSkuAndMerchantCode(ItemServicePriceTest.STORE_ID,
        ItemServicePriceTest.REQUEST_ID, ItemServicePriceTest.USERNAME,
        ItemServicePriceTest.MERCHANT_SKU, ItemServicePriceTest.MERCHANT_CODE, null));
  }

  @Test
  public void addItemPriceByMerchantSkuAndMerchantCodeTestWithStoreIdBlank() {
    this.prices.remove(this.priceWeb);
    Assertions.assertThrows(ApplicationRuntimeException.class, () ->  this.itemServiceImpl.addItemPriceByMerchantSkuAndMerchantCode(null,
        ItemServicePriceTest.REQUEST_ID, ItemServicePriceTest.USERNAME,
        ItemServicePriceTest.MERCHANT_SKU, ItemServicePriceTest.MERCHANT_CODE, this.priceWeb));
  }

  @Test
  public void addItemPriceTest() {
    this.prices.remove(this.priceWeb);
    boolean result =
        this.itemServiceImpl.addItemPrice(ItemServicePriceTest.STORE_ID, this.newPriceWeb,
            ItemServicePriceTest.ITEM_SKU, ItemServicePriceTest.USERNAME);
    verify(this.itemRepository).findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(
        ItemServicePriceTest.STORE_ID, ItemServicePriceTest.ITEM_SKU, false);
    verify(this.saveOperationService).saveItemWithoutUpdatingSolr(this.item, null, false,
        StringUtils.EMPTY, Collections.EMPTY_MAP);
    verify(this.productAndItemSolrIndexerService).updateSolrOnPriceChange(Arrays.asList(this.item));
    assertTrue(result);
  }

  @Test
  public void addItemPriceEventTypeTest() {
    this.prices.remove(this.priceWeb);
    boolean result =
        this.itemServiceImpl.addItemPrice(ItemServicePriceTest.STORE_ID, this.newPriceWeb,
            ItemServicePriceTest.ITEM_SKU, ItemServicePriceTest.USERNAME);
    verify(this.itemRepository).findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(
        ItemServicePriceTest.STORE_ID, ItemServicePriceTest.ITEM_SKU, false);
    verify(this.saveOperationService).saveItemWithoutUpdatingSolr(itemArgumentCaptor.capture(), Mockito.any(), anyBoolean(), anyString(), eq(Collections.EMPTY_MAP));
    verify(this.productAndItemSolrIndexerService).updateSolrOnPriceChange(Arrays.asList(this.item));
    Assertions.assertEquals(1, itemArgumentCaptor.getValue().getItemChangeEventTypes().size());
    Assertions.assertEquals(ItemChangeEventType.ITEM_PRICE_CHANGE, itemArgumentCaptor.getValue().getItemChangeEventTypes().get(0));
    assertTrue(result);
  }

  @Test
  public void addItemPriceTestWithChannelExist() {
    try {
      this.itemServiceImpl.addItemPrice(ItemServicePriceTest.STORE_ID, this.newPriceWeb,
          ItemServicePriceTest.ITEM_SKU, ItemServicePriceTest.USERNAME);
    } catch (Exception e) {
      verify(this.itemRepository).findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(
          ItemServicePriceTest.STORE_ID, ItemServicePriceTest.ITEM_SKU, false);
      assertTrue(e instanceof ApplicationRuntimeException);
    }
  }

  @Test
  public void addItemPriceTestWithItemSkuBlank() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () ->  this.itemServiceImpl.addItemPrice(ItemServicePriceTest.STORE_ID, this.newPriceWeb, null,
        ItemServicePriceTest.USERNAME));
  }

  @Test
  public void addItemPriceTestWithItemSkuNotFound() {
    try {
      this.itemServiceImpl.addItemPrice(ItemServicePriceTest.STORE_ID, this.newPriceWeb,
          ItemServicePriceTest.ITEM_SKU_NOT_FOUND, ItemServicePriceTest.USERNAME);
    } catch (Exception e) {
      verify(this.itemRepository).findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(
          ItemServicePriceTest.STORE_ID, ItemServicePriceTest.ITEM_SKU_NOT_FOUND, false);
      assertTrue(e instanceof Exception);
    }
  }

  @Test
  public void addItemPriceTestWithNullPrice() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () ->  this.itemServiceImpl.addItemPrice(ItemServicePriceTest.STORE_ID, null,
        ItemServicePriceTest.ITEM_SKU, ItemServicePriceTest.USERNAME));
  }

  @Test
  public void addItemPriceWithStoreIdBlank() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () ->  this.itemServiceImpl.addItemPrice(null, this.newPriceWeb, ItemServicePriceTest.ITEM_SKU,
        ItemServicePriceTest.USERNAME));
  }

  @Test
  public void deleteItemPriceByChannelTest() throws Exception {
    this.itemServiceImpl.deleteItemPrice(ItemServicePriceTest.STORE_ID,
        ItemServicePriceTest.ITEM_SKU, ItemServicePriceTest.CHANNEL_WEB);

    verify(this.itemRepository).findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(
        ItemServicePriceTest.STORE_ID, ItemServicePriceTest.ITEM_SKU, false);
    verify(this.objectConverterService).convertToPriceHistory(this.priceWeb,
        ItemServicePriceTest.ITEM_SKU);
    verify(this.priceHistoryService).savePriceHistory(this.priceHistory);
    verify(this.saveOperationService).saveItemWithoutUpdatingSolr(this.item, null, false, StringUtils.EMPTY, Collections.EMPTY_MAP);
    verify(this.productAndItemSolrIndexerService).updateSolrOnPriceChange(Arrays.asList(this.item));
    verify(this.channelService).getDefaultChannel();
  }

  @Test
  public void deleteItemPriceByChannelEventTypeTest() throws Exception {
    this.itemServiceImpl.deleteItemPrice(ItemServicePriceTest.STORE_ID,
        ItemServicePriceTest.ITEM_SKU, ItemServicePriceTest.CHANNEL_WEB);

    verify(this.itemRepository).findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(
        ItemServicePriceTest.STORE_ID, ItemServicePriceTest.ITEM_SKU, false);
    verify(this.objectConverterService).convertToPriceHistory(this.priceWeb,
        ItemServicePriceTest.ITEM_SKU);
    verify(this.priceHistoryService).savePriceHistory(this.priceHistory);
    verify(this.saveOperationService).saveItemWithoutUpdatingSolr(itemArgumentCaptor.capture(), Mockito.any(), anyBoolean(), anyString(), eq(Collections.EMPTY_MAP));
    verify(this.productAndItemSolrIndexerService).updateSolrOnPriceChange(Arrays.asList(this.item));
    verify(this.channelService).getDefaultChannel();
    Assertions.assertEquals(1, itemArgumentCaptor.getValue().getItemChangeEventTypes().size());
    Assertions.assertEquals(ItemChangeEventType.ITEM_PRICE_CHANGE, itemArgumentCaptor.getValue().getItemChangeEventTypes().get(0));
  }

  @Test
  public void deleteItemPriceByChannelTestWithChannelNotFound() throws Exception {
    try {
      this.itemServiceImpl.deleteItemPrice(ItemServicePriceTest.STORE_ID,
          ItemServicePriceTest.ITEM_SKU, ItemServicePriceTest.CHANNEL_MOBILE);
    } catch (Exception e) {
      verify(this.itemRepository).findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(
          ItemServicePriceTest.STORE_ID, ItemServicePriceTest.ITEM_SKU, false);
      verify(this.channelService).getDefaultChannel();
    }
  }

  @Test
  public void deleteItemPriceByChannelTestWithChannelNull() throws Exception {
    Assertions.assertThrows(ApplicationRuntimeException.class, () ->  this.itemServiceImpl.deleteItemPrice(ItemServicePriceTest.STORE_ID,
        ItemServicePriceTest.ITEM_SKU, null));
  }

  @Test
  public void deleteItemPriceByChannelTestWithDefaultChannel() throws Exception {
    try {
      this.itemServiceImpl.deleteItemPrice(ItemServicePriceTest.STORE_ID,
          ItemServicePriceTest.ITEM_SKU, ItemServicePriceTest.CHANNEL_DEFAULT);
    } catch (Exception e) {
      verify(this.channelService).getDefaultChannel();
    }
  }

  @Test
  public void deleteItemPriceByChannelTestWithItemSkuBlank() throws Exception {
    Assertions.assertThrows(ApplicationRuntimeException.class, () ->  this.itemServiceImpl.deleteItemPrice(ItemServicePriceTest.STORE_ID, null,
        ItemServicePriceTest.CHANNEL_WEB));
  }

  @Test
  public void deleteItemPriceByChannelTestWithItemSkuNotFound() throws Exception {
    try {
      this.itemServiceImpl.deleteItemPrice(ItemServicePriceTest.STORE_ID,
          ItemServicePriceTest.ITEM_SKU_NOT_FOUND, ItemServicePriceTest.CHANNEL_WEB);
    } catch (Exception e) {
      verify(this.itemRepository).findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(
          ItemServicePriceTest.STORE_ID, ItemServicePriceTest.ITEM_SKU_NOT_FOUND, false);
      assertTrue(e instanceof ApplicationRuntimeException);
      verify(this.channelService).getDefaultChannel();
    }
  }

  @Test
  public void deleteItemPriceByChannelTestWithStoreIdBlank() throws Exception {
    Assertions.assertThrows(ApplicationRuntimeException.class, () ->  this.itemServiceImpl.deleteItemPrice(null, ItemServicePriceTest.ITEM_SKU,
        ItemServicePriceTest.CHANNEL_WEB));
  }

  @BeforeEach
  public void setUp() throws Exception {
    openMocks(this);
    when(this.channelService.getDefaultChannel()).thenReturn(ChannelName.DEFAULT.toString());
    when(this.skuValidator.isItemSku(ItemServicePriceTest.ITEM_SKU)).thenReturn(true);
    when(
        this.skuValidator.isItemSku(ItemServicePriceTest.ITEM_SKU_FOR_PRICE_WITH_CHANNEL_NOT_FOUND))
        .thenReturn(true);
    when(this.skuValidator.isItemSku(ItemServicePriceTest.ITEM_SKU_NOT_FOUND)).thenReturn(true);
    when(this.skuValidator.isItemSku(ItemServicePriceTest.ITEM_SKU_WITH_EMPTY_PRICE)).thenReturn(
        true);
    this.discountPrice = new DiscountPrice();
    this.discountPrice.setDiscountPrice(ItemServicePriceTest.SALE_PRICE);
    this.discountPrice.setStartDateTime(ItemServicePriceTest.START_DATE_TIME);
    this.discountPrice.setEndDateTime(ItemServicePriceTest.END_DATE_TIME);

    this.priceDefault = new Price();
    this.priceDefault.setCurrency(ItemServicePriceTest.CURRENCY);
    this.priceDefault.setOfferPrice(ItemServicePriceTest.OFFER_PRICE);
    this.priceDefault.setListPrice(ItemServicePriceTest.LIST_PRICE);
    this.priceDefault.setChannel(ItemServicePriceTest.CHANNEL_DEFAULT);

    this.discountPriceList = new ArrayList<DiscountPrice>();
    this.discountPriceList.add(this.discountPrice);

    this.priceWeb = new Price();
    this.priceWeb.setCurrency(ItemServicePriceTest.CURRENCY);
    this.priceWeb.setOfferPrice(ItemServicePriceTest.OFFER_PRICE);
    this.priceWeb.setListPrice(ItemServicePriceTest.LIST_PRICE);
    this.priceWeb.setChannel(ItemServicePriceTest.CHANNEL_WEB);
    this.priceWeb.setListOfDiscountPrices(this.discountPriceList);

    this.newPriceWeb = new Price();
    this.newPriceWeb.setChannel(ItemServicePriceTest.CHANNEL_WEB);

    this.itemPriceWithChannelNotFound = new Price();
    this.itemPriceWithChannelNotFound.setChannel(ItemServicePriceTest.CHANNEL_MOBILE);

    this.prices = new HashSet<Price>();
    this.prices.add(this.priceDefault);
    this.prices.add(this.priceWeb);

    this.item = new Item();
    this.item.setItemSku(ItemServicePriceTest.ITEM_SKU);
    this.item.setProductSku(ItemServicePriceTest.PRODUCT_SKU);
    this.item.setItemCode(ItemServicePriceTest.ITEM_CODE);
    this.item.setSynchronized(ItemServicePriceTest.IS_SYNCHRONIZED);
    this.item.setPrice(this.prices);

    this.itemWithDifferentProductSku = new Item();

    this.itemWithEmptyPrice = new Item();
    this.itemWithEmptyPrice.setProductSku(ItemServicePriceTest.PRODUCT_SKU);

    this.listOfItems = new ArrayList<Item>();
    this.listOfItems.add(this.item);
    this.listOfItems.add(this.itemWithDifferentProductSku);
    this.listOfItems.add(this.itemWithEmptyPrice);

    this.priceHistory = new PriceHistory();

    this.product = new Product();
    this.product.setProductSku(ItemServicePriceTest.PRODUCT_SKU);

    this.setOfProducts = new ArrayList<>();
    this.setOfProducts.add(this.product);

    when(
        this.itemRepository.findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(
            ItemServicePriceTest.STORE_ID, ItemServicePriceTest.ITEM_SKU, false)).thenReturn(this.item);

    when(
        this.objectConverterService.convertToPriceHistory(this.priceWeb,
            ItemServicePriceTest.ITEM_SKU)).thenReturn(this.priceHistory);

    when(
        this.itemRepository.findItemsByStoreIdAndMerchantSkuAndMarkForDeleteFalse(
            ItemServicePriceTest.STORE_ID, ItemServicePriceTest.MERCHANT_SKU)).thenReturn(
        this.listOfItems);

    when(
        this.itemRepository.findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(
            ItemServicePriceTest.STORE_ID,
            ItemServicePriceTest.ITEM_SKU_FOR_PRICE_WITH_CHANNEL_NOT_FOUND, false)).thenReturn(this.item);

    when(
        this.productService.getProductsByMerchantCode(ItemServicePriceTest.STORE_ID,
            ItemServicePriceTest.MERCHANT_CODE)).thenReturn(this.setOfProducts);

    this.emptySetOfProducts = new ArrayList<>();
    when(
        this.productService.getProductsByMerchantCode(ItemServicePriceTest.STORE_ID,
            ItemServicePriceTest.MERCHANT_CODE_NOT_FOUND)).thenReturn(this.emptySetOfProducts);

    when(this.itemRepository.save(this.item)).thenReturn(this.item);

    when(this.itemRepository.saveAll(this.argumentCaptorListOfItems.capture())).thenReturn(
        this.listOfItems);
    when(channelService.getDefaultChannel()).thenReturn(CHANNEL_DEFAULT);
  }

  @AfterEach
  public void tearDown() {
    verifyNoMoreInteractions(this.objectConverterService);
    verifyNoMoreInteractions(this.itemRepository);
    verifyNoMoreInteractions(this.priceHistoryService);
    verifyNoMoreInteractions(this.productService);
    verifyNoMoreInteractions(this.saveOperationService);
    verifyNoMoreInteractions(this.channelService);
    verifyNoMoreInteractions(this.skuValidator);
    verifyNoMoreInteractions(this.itemHelperService, productAndItemSolrIndexerService);
    verifyNoMoreInteractions(this.itemPickupPointService);
  }

  @Test
  public void updateItemPriceByMerchantSkuAndMerchantCode() throws Exception {
    boolean result =
        this.itemServiceImpl.updateItemPriceByMerchantSkuAndMerchantCode(
            ItemServicePriceTest.STORE_ID, ItemServicePriceTest.REQUEST_ID,
            ItemServicePriceTest.USERNAME, ItemServicePriceTest.MERCHANT_SKU,
            ItemServicePriceTest.MERCHANT_CODE, this.newPriceWeb);
    verify(this.itemRepository).findItemsByStoreIdAndMerchantSkuAndMarkForDeleteFalse(
        ItemServicePriceTest.STORE_ID, ItemServicePriceTest.MERCHANT_SKU);

    verify(this.productService).getProductsByMerchantCode(ItemServicePriceTest.STORE_ID,
        ItemServicePriceTest.MERCHANT_CODE);

    verify(this.objectConverterService)
        .convertToPriceHistory(this.priceWeb, this.item.getItemSku());

    verify(this.priceHistoryService).savePriceHistory(this.priceHistory);

    verify(this.saveOperationService).saveItemsAndClearCacheWithoutUpdatingSolr(this.argumentCaptorListOfItems.capture(),
        Mockito.any(), anyString());
    verify(this.productAndItemSolrIndexerService).updateSolrOnPriceChange(this.argumentCaptorListOfItems.capture());
    assertTrue(result);
  }

  @Test
  public void updateItemPriceByMerchantSkuAndMerchantCodeEventTypeTest() throws Exception {
    boolean result =
        this.itemServiceImpl.updateItemPriceByMerchantSkuAndMerchantCode(
            ItemServicePriceTest.STORE_ID, ItemServicePriceTest.REQUEST_ID,
            ItemServicePriceTest.USERNAME, ItemServicePriceTest.MERCHANT_SKU,
            ItemServicePriceTest.MERCHANT_CODE, this.newPriceWeb);
    verify(this.itemRepository).findItemsByStoreIdAndMerchantSkuAndMarkForDeleteFalse(
        ItemServicePriceTest.STORE_ID, ItemServicePriceTest.MERCHANT_SKU);

    verify(this.productService).getProductsByMerchantCode(ItemServicePriceTest.STORE_ID,
        ItemServicePriceTest.MERCHANT_CODE);

    verify(this.objectConverterService)
        .convertToPriceHistory(this.priceWeb, this.item.getItemSku());

    verify(this.priceHistoryService).savePriceHistory(this.priceHistory);

    verify(this.saveOperationService).saveItemsAndClearCacheWithoutUpdatingSolr(this.argumentCaptorListOfItems.capture(),
        Mockito.any(), anyString());
    verify(this.productAndItemSolrIndexerService).updateSolrOnPriceChange(this.argumentCaptorListOfItems.capture());
    Assertions.assertEquals(1, argumentCaptorListOfItems.getValue().get(0).getItemChangeEventTypes().size());
    Assertions.assertEquals(ItemChangeEventType.ITEM_PRICE_CHANGE, argumentCaptorListOfItems.getValue().get(0).getItemChangeEventTypes().get(0));
    assertTrue(result);
  }

  @Test
  public void updateItemPriceByMerchantSkuAndMerchantCode_WhenMerchantPromoDiscountTrueTest() throws Exception {
    this.item.setMerchantPromoDiscount(true);
    this.itemWithDifferentProductSku.setMerchantPromoDiscount(true);
    this.itemWithEmptyPrice.setMerchantPromoDiscount(true);
    this.listOfItems.add(this.item);
    this.listOfItems.add(this.itemWithDifferentProductSku);
    this.listOfItems.add(this.itemWithEmptyPrice);
    boolean result =
        this.itemServiceImpl.updateItemPriceByMerchantSkuAndMerchantCode(
            ItemServicePriceTest.STORE_ID, ItemServicePriceTest.REQUEST_ID,
            ItemServicePriceTest.USERNAME, ItemServicePriceTest.MERCHANT_SKU,
            ItemServicePriceTest.MERCHANT_CODE, this.newPriceWeb);
    verify(this.itemRepository).findItemsByStoreIdAndMerchantSkuAndMarkForDeleteFalse(
        ItemServicePriceTest.STORE_ID, ItemServicePriceTest.MERCHANT_SKU);
    verify(this.productService).getProductsByMerchantCode(ItemServicePriceTest.STORE_ID,
        ItemServicePriceTest.MERCHANT_CODE);
    verify(this.saveOperationService).saveItemsAndClearCacheWithoutUpdatingSolr(this.argumentCaptorListOfItems.capture(),
        Mockito.any(), anyString());
    verify(this.productAndItemSolrIndexerService).updateSolrOnPriceChange(this.argumentCaptorListOfItems.capture());
    assertTrue(result);
  }


  @Test
  public void updateItemPriceByMerchantSkuAndMerchantCodeWithNullItem() throws Exception {
    Assertions.assertThrows(ApplicationRuntimeException.class, () ->  this.itemServiceImpl.updateItemPriceByMerchantSkuAndMerchantCode(ItemServicePriceTest.STORE_ID,
        ItemServicePriceTest.REQUEST_ID, ItemServicePriceTest.USERNAME,
        ItemServicePriceTest.MERCHANT_SKU, ItemServicePriceTest.MERCHANT_CODE, null));
  }

  @Test
  public void updateItemPriceByMerchantSkuAndMerchantCodeWithNullMerchantCode() throws Exception {
    Assertions.assertThrows(ApplicationRuntimeException.class, () ->  this.itemServiceImpl.updateItemPriceByMerchantSkuAndMerchantCode(ItemServicePriceTest.STORE_ID,
        ItemServicePriceTest.REQUEST_ID, ItemServicePriceTest.USERNAME,
        ItemServicePriceTest.MERCHANT_SKU, null, this.newPriceWeb));
  }

  @Test
  public void updateItemPriceByMerchantSkuAndMerchantCodeWithNullStoreId() throws Exception {
    Assertions.assertThrows(ApplicationRuntimeException.class,
        () -> this.itemServiceImpl.updateItemPriceByMerchantSkuAndMerchantCode(null, ItemServicePriceTest.REQUEST_ID,
            ItemServicePriceTest.USERNAME, ItemServicePriceTest.MERCHANT_SKU, ItemServicePriceTest.MERCHANT_CODE,
            this.newPriceWeb));
  }

  @Test
  public void updateItemPriceTest() throws Exception {
    Set<Price> prices = new HashSet<Price>();
    prices.add(this.newPriceWeb);
    ItemPickupPoint itemPickupPoint = new ItemPickupPoint();
    itemPickupPoint.setPrice(item.getPrice());
    when(itemPickupPointService.findByItemSkuAndDelivery(Mockito.anyString(), Mockito.anyString())).thenReturn(
        itemPickupPoint);
    when(this.saveOperationService.saveItemsAndClearCacheWithoutUpdatingSolr(Mockito.anyList(), Mockito.any(), anyString()))
        .thenReturn(Arrays.asList(item));
    when(objectConverterService.convertToItemPickupPointChangeEventModel(Mockito.any(),
        Mockito.anyBoolean())).thenReturn(new ItemPickupPointDataChangeEventModel());
    when(this.itemHelperService.setItemPriceByChannel(this.item, prices, ItemServicePriceTest.USERNAME))
        .thenReturn(this.item);
    boolean result =
        this.itemServiceImpl.updateItemPrice(ItemServicePriceTest.USERNAME,
            ItemServicePriceTest.STORE_ID, this.newPriceWeb, ItemServicePriceTest.ITEM_SKU, null);
    verify(this.skuValidator).isItemSku(ItemServicePriceTest.ITEM_SKU);
    verify(this.itemRepository).findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(
        ItemServicePriceTest.STORE_ID, ItemServicePriceTest.ITEM_SKU, false);
    verify(this.itemHelperService).setItemPriceByChannel(this.item, prices, ItemServicePriceTest.USERNAME);
    verify(this.productAndItemSolrIndexerService).updateSolrOnPriceAndWholesalePriceFlagChange(item, null);
    assertTrue(result);
    assertFalse(item.isContentChanged());
    verify(this.itemPickupPointService).findByItemSkuAndDelivery(Mockito.anyString(), Mockito.anyString());
    verify(this.itemPickupPointService).saveItemPickupPoint(Mockito.any(ItemPickupPoint.class));
    verify(this.objectConverterService).overrideL4DetailsFromL5(Mockito.anyList(), Mockito.anyList());
    verify(this.objectConverterService).convertToItemPickupPointChangeEventModel(Mockito.isNull(),
        Mockito.anyBoolean());
    verify(this.cacheEvictHelperService).evictItemPickupPointData(Mockito.anyString(), Mockito.any(),
        Mockito.isNull());
    verify(channelService).getDefaultChannel();
    verify(this.cacheEvictHelperService).evictItemCache(Mockito.anyString(), eq(item));
    verify(this.saveAndPublishService).publishListOfItems(Mockito.anyList(), Mockito.anyList(), Mockito.anyList(),
      anyBoolean());
  }

  @Test
  public void updateItemPriceEventTypeTest() throws Exception {
    Set<Price> prices = new HashSet<Price>();
    prices.add(this.newPriceWeb);
    ItemPickupPoint itemPickupPoint = new ItemPickupPoint();
    itemPickupPoint.setPrice(item.getPrice());
    when(itemPickupPointService.findByItemSkuAndDelivery(Mockito.anyString(), Mockito.anyString())).thenReturn(
        itemPickupPoint);
    when(this.saveOperationService.saveItemsAndClearCacheWithoutUpdatingSolr(Mockito.anyList(), Mockito.any(), anyString()))
        .thenReturn(Arrays.asList(item));
    when(this.itemHelperService.setItemPriceByChannel(this.item, prices, ItemServicePriceTest.USERNAME))
        .thenReturn(this.item);
    when(objectConverterService.convertToItemPickupPointChangeEventModel(Mockito.any(),
        Mockito.anyBoolean())).thenReturn(new ItemPickupPointDataChangeEventModel());
    boolean result =
        this.itemServiceImpl.updateItemPrice(ItemServicePriceTest.USERNAME,
            ItemServicePriceTest.STORE_ID, this.newPriceWeb, ItemServicePriceTest.ITEM_SKU, null);
    verify(this.skuValidator).isItemSku(ItemServicePriceTest.ITEM_SKU);
    verify(this.itemRepository).findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(
        ItemServicePriceTest.STORE_ID, ItemServicePriceTest.ITEM_SKU, false);
    verify(this.itemHelperService).setItemPriceByChannel(this.item, prices, ItemServicePriceTest.USERNAME);
    verify(this.productAndItemSolrIndexerService).updateSolrOnPriceAndWholesalePriceFlagChange(item, null);
    verify(this.itemPickupPointService).findByItemSkuAndDelivery(Mockito.anyString(), Mockito.anyString());
    verify(this.itemPickupPointService).saveItemPickupPoint(Mockito.any(ItemPickupPoint.class));
    verify(this.objectConverterService).overrideL4DetailsFromL5(Mockito.anyList(), Mockito.anyList());
    verify(this.objectConverterService).convertToItemPickupPointChangeEventModel(Mockito.isNull(),
        Mockito.anyBoolean());
    verify(this.cacheEvictHelperService).evictItemPickupPointData(Mockito.anyString(), Mockito.any(),
        Mockito.isNull());
    verify(channelService).getDefaultChannel();
    verify(this.cacheEvictHelperService).evictItemCache(Mockito.anyString(), eq(item));
    verify(this.saveAndPublishService).publishListOfItems(Mockito.anyList(), Mockito.anyList(),Mockito.anyList(),
      anyBoolean());
    assertTrue(result);
  }


  @Test
  public void updateItemPriceTestWithEmptyPrice() {
    try {
      this.itemServiceImpl.updateItemPrice(ItemServicePriceTest.USERNAME,
          ItemServicePriceTest.STORE_ID, this.newPriceWeb,
          ItemServicePriceTest.ITEM_SKU_WITH_EMPTY_PRICE, null);
    } catch (Exception e) {
      verify(this.itemRepository).findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(
          ItemServicePriceTest.STORE_ID, ItemServicePriceTest.ITEM_SKU_WITH_EMPTY_PRICE, false);
      verify(this.skuValidator).isItemSku(ItemServicePriceTest.ITEM_SKU_WITH_EMPTY_PRICE);
      assertTrue(e instanceof Exception);
    }
  }

  @Test
  public void updateItemPriceTestWithItemSkuBlank() throws Exception {
    Assertions.assertThrows(ApplicationRuntimeException.class, () ->  this.itemServiceImpl.updateItemPrice(ItemServicePriceTest.USERNAME, null, this.newPriceWeb,
        ItemServicePriceTest.ITEM_SKU, null));
  }

  @Test
  public void updateItemPriceTestWithNullItem() {
    try {
      this.itemServiceImpl.updateItemPrice(ItemServicePriceTest.USERNAME,
          ItemServicePriceTest.STORE_ID, this.newPriceWeb, ItemServicePriceTest.ITEM_SKU_NOT_FOUND, null);
    } catch (Exception e) {
      verify(this.itemRepository).findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(
          ItemServicePriceTest.STORE_ID, ItemServicePriceTest.ITEM_SKU_NOT_FOUND, false);
      verify(this.skuValidator).isItemSku(ItemServicePriceTest.ITEM_SKU_NOT_FOUND);
      assertTrue(e instanceof Exception);
    }
  }

  @Test
  public void updateItemPriceTestWithNullPrice() throws Exception {
    try {
      this.itemServiceImpl.updateItemPrice(ItemServicePriceTest.USERNAME,
          ItemServicePriceTest.STORE_ID, null, ItemServicePriceTest.ITEM_SKU, null);
    } catch (Exception e) {
      verify(this.skuValidator).isItemSku(ItemServicePriceTest.ITEM_SKU);
    }
  }

  @Test
  public void updateItemPriceTest_isForceReviewTrue() throws Exception {
    this.item.setForceReview(true);
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () ->  this.itemServiceImpl
          .updateItemPrice(ItemServicePriceTest.USERNAME, ItemServicePriceTest.STORE_ID, this.newPriceWeb,
              ItemServicePriceTest.ITEM_SKU, null));
    } catch (ApplicationRuntimeException e) {
      throw(e);
    } finally {
      verify(this.itemRepository).findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(
          ItemServicePriceTest.STORE_ID, ItemServicePriceTest.ITEM_SKU, false);
      verify(this.skuValidator).isItemSku(ItemServicePriceTest.ITEM_SKU);
    }
  }

  @Test
  public void updateItemPriceTest_wholesaleActivated() throws Exception {
    Set<Price> prices = new HashSet<Price>();
    prices.add(this.newPriceWeb);
    ItemPickupPoint itemPickupPoint = new ItemPickupPoint();
    itemPickupPoint.setPrice(item.getPrice());
    when(itemPickupPointService.findByItemSkuAndDelivery(Mockito.anyString(), Mockito.anyString())).thenReturn(
        itemPickupPoint);
    when(this.saveOperationService.saveItemsAndClearCacheWithoutUpdatingSolr(Mockito.anyList(), Mockito.any(), anyString()))
        .thenReturn(Arrays.asList(item));
    when(objectConverterService.convertToItemPickupPointChangeEventModel(Mockito.any(),
        Mockito.anyBoolean())).thenReturn(new ItemPickupPointDataChangeEventModel());
    when(this.itemHelperService.setItemPriceByChannel(this.item, prices, ItemServicePriceTest.USERNAME))
        .thenReturn(this.item);
    Mockito.doNothing().when(this.productAndItemSolrIndexerService)
        .updateSolrOnPriceAndWholesalePriceFlagChange(item, true);
    boolean result =
        this.itemServiceImpl.updateItemPrice(ItemServicePriceTest.USERNAME,
            ItemServicePriceTest.STORE_ID, this.newPriceWeb, ItemServicePriceTest.ITEM_SKU, true);
    verify(this.skuValidator).isItemSku(ItemServicePriceTest.ITEM_SKU);
    verify(this.itemRepository).findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(
        ItemServicePriceTest.STORE_ID, ItemServicePriceTest.ITEM_SKU, false);
    verify(this.productAndItemSolrIndexerService).updateSolrOnPriceAndWholesalePriceFlagChange(item, true);
    verify(this.itemHelperService).setItemPriceByChannel(this.item, prices, ItemServicePriceTest.USERNAME);
    assertTrue(result);
    verify(this.itemPickupPointService).findByItemSkuAndDelivery(Mockito.anyString(), Mockito.anyString());
    verify(this.itemPickupPointService).saveItemPickupPoint(Mockito.any(ItemPickupPoint.class));
    verify(this.objectConverterService).overrideL4DetailsFromL5(Mockito.anyList(), Mockito.anyList());
    verify(this.objectConverterService).convertToItemPickupPointChangeEventModel(Mockito.isNull(),
        Mockito.anyBoolean());
    verify(this.cacheEvictHelperService).evictItemPickupPointData(Mockito.anyString(), Mockito.any(),
        Mockito.isNull());
    verify(channelService).getDefaultChannel();
    verify(this.cacheEvictHelperService).evictItemCache(Mockito.anyString(), eq(item));
    verify(this.saveAndPublishService).publishListOfItems(Mockito.anyList(), Mockito.anyList(), Mockito.anyList(),
      anyBoolean());
  }

  @Test
  public void updateItemPriceTest_wholesaleActivated_falseTest() throws Exception {
    Set<Price> prices = new HashSet<Price>();
    prices.add(this.newPriceWeb);
    ItemPickupPoint itemPickupPoint = new ItemPickupPoint();
    itemPickupPoint.setPrice(item.getPrice());
    itemPickupPoint.setActivePromoBundlings(Sets.newHashSet(Constants.COMBO));
    this.item.setActivePromoBundlings(Sets.newHashSet(Constants.COMBO));
    when(itemPickupPointService.findByItemSkuAndDelivery(Mockito.anyString(), Mockito.anyString())).thenReturn(
        itemPickupPoint);
    when(this.saveOperationService.saveItemsAndClearCacheWithoutUpdatingSolr(Mockito.anyList(), Mockito.any(), anyString()))
        .thenReturn(Arrays.asList(item));
    when(objectConverterService.convertToItemPickupPointChangeEventModel(Mockito.any(),
        Mockito.anyBoolean())).thenReturn(new ItemPickupPointDataChangeEventModel());
    when(this.itemHelperService.setItemPriceByChannel(this.item, prices, ItemServicePriceTest.USERNAME))
        .thenReturn(this.item);
    Mockito.doNothing().when(this.productAndItemSolrIndexerService)
        .updateSolrOnPriceAndWholesalePriceFlagChange(item, true);
    boolean result =
        this.itemServiceImpl.updateItemPrice(ItemServicePriceTest.USERNAME,
            ItemServicePriceTest.STORE_ID, this.newPriceWeb, ItemServicePriceTest.ITEM_SKU, false);
    verify(this.skuValidator).isItemSku(ItemServicePriceTest.ITEM_SKU);
    verify(this.itemRepository).findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(
        ItemServicePriceTest.STORE_ID, ItemServicePriceTest.ITEM_SKU, false);
    verify(this.itemHelperService).setItemPriceByChannel(this.item, prices, ItemServicePriceTest.USERNAME);
    verify(this.productAndItemSolrIndexerService).updateSolrOnPriceAndWholesalePriceFlagChange(item, null);
    assertTrue(result);
    verify(this.itemPickupPointService).findByItemSkuAndDelivery(Mockito.anyString(), Mockito.anyString());
    verify(this.itemPickupPointService).saveItemPickupPoint(Mockito.any(ItemPickupPoint.class));
    verify(this.objectConverterService).overrideL4DetailsFromL5(Mockito.anyList(), Mockito.anyList());
    verify(this.objectConverterService).convertToItemPickupPointChangeEventModel(Mockito.isNull(),
        Mockito.anyBoolean());
    verify(this.cacheEvictHelperService).evictItemPickupPointData(Mockito.anyString(), Mockito.any(),
        Mockito.isNull());
    verify(channelService).getDefaultChannel();
    verify(this.cacheEvictHelperService).evictItemCache(Mockito.anyString(), eq(item));
    verify(this.saveAndPublishService).publishListOfItems(Mockito.anyList(), Mockito.anyList(), Mockito.anyList(),
      anyBoolean());

  }

  @Test
  public void updateItemPriceTest_wholesaleActivated_TrueTest() throws Exception {
    Set<Price> prices = new HashSet<Price>();
    prices.add(this.newPriceWeb);
    ItemPickupPoint itemPickupPoint = new ItemPickupPoint();
    itemPickupPoint.setPrice(item.getPrice());
    itemPickupPoint.setActivePromoBundlings(Sets.newHashSet(Constants.COMBO));
    item.setActivePromoBundlings(Sets.newHashSet(Constants.COMBO));
    when(itemPickupPointService.findByItemSkuAndDelivery(Mockito.anyString(), Mockito.anyString())).thenReturn(
        itemPickupPoint);
    when(this.saveOperationService.saveItemsAndClearCacheWithoutUpdatingSolr(Mockito.anyList(), Mockito.any(), anyString()))
        .thenReturn(Arrays.asList(item));
    when(objectConverterService.convertToItemPickupPointChangeEventModel(Mockito.any(),
        Mockito.anyBoolean())).thenReturn(new ItemPickupPointDataChangeEventModel());
    when(this.itemHelperService.setItemPriceByChannel(this.item, prices, ItemServicePriceTest.USERNAME))
        .thenReturn(this.item);
    Mockito.doNothing().when(this.productAndItemSolrIndexerService)
        .updateSolrOnPriceAndWholesalePriceFlagChange(item, true);
    boolean result =
        this.itemServiceImpl.updateItemPrice(ItemServicePriceTest.USERNAME,
            ItemServicePriceTest.STORE_ID, this.newPriceWeb, ItemServicePriceTest.ITEM_SKU, true);
    verify(this.skuValidator).isItemSku(ItemServicePriceTest.ITEM_SKU);
    verify(this.itemRepository).findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(
        ItemServicePriceTest.STORE_ID, ItemServicePriceTest.ITEM_SKU, false);
    verify(this.itemHelperService).setItemPriceByChannel(this.item, prices, ItemServicePriceTest.USERNAME);
    verify(this.productAndItemSolrIndexerService).updateSolrOnPriceAndWholesalePriceFlagChange(item, true);
    assertTrue(result);
    verify(channelService).getDefaultChannel();
    verify(this.itemPickupPointService).findByItemSkuAndDelivery(Mockito.anyString(), Mockito.anyString());
    verify(this.itemPickupPointService).saveItemPickupPoint(Mockito.any(ItemPickupPoint.class));
    verify(this.objectConverterService).overrideL4DetailsFromL5(Mockito.anyList(), Mockito.anyList());
    verify(this.objectConverterService).convertToItemPickupPointChangeEventModel(Mockito.isNull(),
        Mockito.anyBoolean());
    verify(this.cacheEvictHelperService).evictItemPickupPointData(Mockito.anyString(), Mockito.any(),
        Mockito.isNull());
    verify(this.cacheEvictHelperService).evictItemCache(Mockito.anyString(), eq(item));
    verify(this.saveAndPublishService).publishListOfItems(Mockito.anyList(), Mockito.anyList(), Mockito.anyList(),
      anyBoolean());

  }

  @Test
  public void updateItemPriceTest_wholesaleActivated_nullTest() throws Exception {
    Set<Price> prices = new HashSet<Price>();
    prices.add(this.newPriceWeb);
    ItemPickupPoint itemPickupPoint = new ItemPickupPoint();
    itemPickupPoint.setPrice(item.getPrice());
    itemPickupPoint.setActivePromoBundlings(Sets.newHashSet(Constants.WHOLESALE_PRICE));
    when(itemPickupPointService.findByItemSkuAndDelivery(Mockito.anyString(), Mockito.anyString())).thenReturn(
        itemPickupPoint);
    when(objectConverterService.convertToItemPickupPointChangeEventModel(Mockito.any(),
        Mockito.anyBoolean())).thenReturn(new ItemPickupPointDataChangeEventModel());
    when(this.saveOperationService.saveItemsAndClearCacheWithoutUpdatingSolr(Mockito.anyList(), Mockito.any(), anyString()))
        .thenReturn(Arrays.asList(item));
    when(this.itemHelperService.setItemPriceByChannel(this.item, prices, ItemServicePriceTest.USERNAME))
        .thenReturn(this.item);
    Mockito.doNothing().when(this.productAndItemSolrIndexerService)
        .updateSolrOnPriceAndWholesalePriceFlagChange(item, true);
    boolean result =
        this.itemServiceImpl.updateItemPrice(ItemServicePriceTest.USERNAME,
            ItemServicePriceTest.STORE_ID, this.newPriceWeb, ItemServicePriceTest.ITEM_SKU, true);
    verify(this.skuValidator).isItemSku(ItemServicePriceTest.ITEM_SKU);
    verify(this.itemRepository).findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(
        ItemServicePriceTest.STORE_ID, ItemServicePriceTest.ITEM_SKU, false);
    verify(this.itemHelperService).setItemPriceByChannel(this.item, prices, ItemServicePriceTest.USERNAME);
    verify(this.productAndItemSolrIndexerService).updateSolrOnPriceAndWholesalePriceFlagChange(item, null);
    assertTrue(result);
    verify(this.itemPickupPointService).findByItemSkuAndDelivery(Mockito.anyString(), Mockito.anyString());
    verify(this.itemPickupPointService).saveItemPickupPoint(Mockito.any(ItemPickupPoint.class));
    verify(this.objectConverterService).overrideL4DetailsFromL5(Mockito.anyList(), Mockito.anyList());
    verify(this.objectConverterService).convertToItemPickupPointChangeEventModel(Mockito.isNull(),
        Mockito.anyBoolean());
    verify(this.cacheEvictHelperService).evictItemPickupPointData(Mockito.anyString(), Mockito.any(),
        Mockito.isNull());
    verify(channelService).getDefaultChannel();
    verify(this.cacheEvictHelperService).evictItemCache(Mockito.anyString(), eq(item));
    verify(this.saveAndPublishService).publishListOfItems(Mockito.anyList(), Mockito.anyList(), Mockito.anyList(),
      anyBoolean());

  }

  @Test
  public void updateItemPriceTest_wholesaleDeactivated() throws Exception {
    Set<Price> prices = new HashSet<Price>();
    prices.add(this.newPriceWeb);
    this.item.setActivePromoBundlings(Sets.newHashSet(Constants.WHOLESALE_PRICE));
    ItemPickupPoint itemPickupPoint = new ItemPickupPoint();
    itemPickupPoint.setPrice(item.getPrice());
    itemPickupPoint.setActivePromoBundlings(Sets.newHashSet(Constants.WHOLESALE_PRICE));
    when(itemPickupPointService.findByItemSkuAndDelivery(Mockito.anyString(), Mockito.anyString())).thenReturn(
        itemPickupPoint);
    when(this.saveOperationService.saveItemsAndClearCacheWithoutUpdatingSolr(Mockito.anyList(), Mockito.any(), anyString()))
        .thenReturn(Arrays.asList(item));
    when(this.itemHelperService.setItemPriceByChannel(this.item, prices, ItemServicePriceTest.USERNAME))
        .thenReturn(this.item);
    when(objectConverterService.convertToItemPickupPointChangeEventModel(Mockito.any(),
        Mockito.anyBoolean())).thenReturn(new ItemPickupPointDataChangeEventModel());
    Mockito.doNothing().when(this.productAndItemSolrIndexerService)
        .updateSolrOnPriceAndWholesalePriceFlagChange(item, false);
    boolean result =
        this.itemServiceImpl.updateItemPrice(ItemServicePriceTest.USERNAME,
            ItemServicePriceTest.STORE_ID, this.newPriceWeb, ItemServicePriceTest.ITEM_SKU, false);
    verify(this.skuValidator).isItemSku(ItemServicePriceTest.ITEM_SKU);
    verify(this.itemRepository).findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(
        ItemServicePriceTest.STORE_ID, ItemServicePriceTest.ITEM_SKU, false);
    verify(this.itemHelperService).setItemPriceByChannel(this.item, prices, ItemServicePriceTest.USERNAME);
    verify(this.productAndItemSolrIndexerService).updateSolrOnPriceAndWholesalePriceFlagChange(item, false);
    verify(this.itemPickupPointService).findByItemSkuAndDelivery(Mockito.anyString(), Mockito.anyString());
    verify(this.itemPickupPointService).saveItemPickupPoint(Mockito.any(ItemPickupPoint.class));
    verify(this.objectConverterService).overrideL4DetailsFromL5(Mockito.anyList(), Mockito.anyList());
    verify(this.objectConverterService).convertToItemPickupPointChangeEventModel(Mockito.isNull(),
        Mockito.anyBoolean());
    verify(this.cacheEvictHelperService).evictItemPickupPointData(Mockito.anyString(), Mockito.any(),
        Mockito.isNull());
    verify(this.cacheEvictHelperService).evictItemCache(Mockito.anyString(), eq(item));
    verify(channelService).getDefaultChannel();
    verify(this.saveAndPublishService).publishListOfItems(Mockito.anyList(), Mockito.anyList(), Mockito.anyList(),
      anyBoolean());
    assertTrue(result);
  }

  @Test
  public void updateItemPriceEmptyPickupPointTest() throws Exception {
    Set<Price> prices = new HashSet<Price>();
    prices.add(this.newPriceWeb);
    ItemPickupPoint itemPickupPoint = new ItemPickupPoint();
    itemPickupPoint.setPrice(item.getPrice());
    when(itemPickupPointService.findByItemSkuAndDelivery(Mockito.anyString(), Mockito.anyString())).thenReturn(null);
    when(this.saveOperationService.saveItemsAndClearCacheWithoutUpdatingSolr(Mockito.anyList(), Mockito.any(), anyString())).thenReturn(
        Arrays.asList(item));
    when(this.itemHelperService.setItemPriceByChannel(this.item, prices, ItemServicePriceTest.USERNAME)).thenReturn(
        this.item);
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () ->
          this.itemServiceImpl.updateItemPrice(ItemServicePriceTest.USERNAME, ItemServicePriceTest.STORE_ID,
              this.newPriceWeb, ItemServicePriceTest.ITEM_SKU, null));
    } catch (Exception e) {
      throw e;
    } finally {
      verify(this.skuValidator).isItemSku(ItemServicePriceTest.ITEM_SKU);
      verify(this.itemRepository).findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(ItemServicePriceTest.STORE_ID,
          ItemServicePriceTest.ITEM_SKU, false);
      verify(this.itemPickupPointService).findByItemSkuAndDelivery(Mockito.anyString(), Mockito.anyString());
    }
  }
}
