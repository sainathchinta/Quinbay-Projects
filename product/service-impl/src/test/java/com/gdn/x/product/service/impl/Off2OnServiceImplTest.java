package com.gdn.x.product.service.impl;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;
import static org.mockito.MockitoAnnotations.openMocks;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

import org.apache.commons.lang3.StringUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;

import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.x.product.model.entity.Item;
import com.gdn.x.product.model.entity.ItemPickupPoint;
import com.gdn.x.product.model.entity.Price;
import com.gdn.x.product.model.vo.Off2OnPriceVO;
import com.gdn.x.product.rest.web.model.request.ItemPickupPointRequest;
import com.gdn.x.product.service.api.BusinessPartnerPromoService;
import com.gdn.x.product.service.api.ChannelService;
import com.gdn.x.product.service.api.ItemPickupPointService;
import com.gdn.x.product.service.api.ItemService;
import com.gdn.x.product.service.api.Off2OnHelperService;
import com.gdn.x.product.service.api.ProductHelperService;
import com.google.common.collect.ImmutableMap;
import com.google.common.collect.ImmutableSet;

public class Off2OnServiceImplTest {

  private static final String MERCHANT_CODE = "merchantCode";

  private static final boolean OFF2_ON_CHANNEL_ACTIVE = true;

  private static final int LIST_PRICE = 2;

  private static final int OFFER_PRICE = 1;

  private static final String CURRENCY = "currency";

  private static final String CHANNEL = "channel";

  private static final String BLANK = "";

  private static final String PRODUCT_SKU = "productSku";

  private static final String ITEM_SKU = "itemSku";

  private static final String STORE_ID = "storeId";

  private static final String PICKUP_POINT_CODE = "pickupPointCode";

  @InjectMocks
  private Off2OnServiceImpl service;

  @Mock
  private Off2OnHelperService off2OnHelperService;

  @Mock
  private ProductHelperService productHelperService;

  @Mock
  private ChannelService channelService;

  @Mock
  private ItemService itemService;

  @Mock
  private ItemPickupPointService itemPickupPointService;

  @Mock
  private BusinessPartnerPromoService businessPartnerPromoService;

  @Test
  public void activateOff2OnChannelByItemSkuListTest() {
    List<String> itemSkus = new ArrayList<String>();
    itemSkus.add(Off2OnServiceImplTest.ITEM_SKU);
    when(this.off2OnHelperService.changeOff2OnChannelActiveByItemSku(Off2OnServiceImplTest.STORE_ID, itemSkus,
        true)).thenReturn(itemSkus);
    List<String> result = this.service.activateOff2OnChannelByItemSku(Off2OnServiceImplTest.STORE_ID, itemSkus);
    assertEquals(result, itemSkus);
    verify(this.off2OnHelperService).changeOff2OnChannelActiveByItemSku(Off2OnServiceImplTest.STORE_ID, itemSkus, true);
  }

  @Test
  public void activateOff2OnChannelByItemSkuTest() {
    when(this.off2OnHelperService.changeOff2OnChannelActiveByItemSku(Off2OnServiceImplTest.STORE_ID,
        Off2OnServiceImplTest.ITEM_SKU, true)).thenReturn(true);
    boolean result =
        this.service.activateOff2OnChannelByItemSku(Off2OnServiceImplTest.STORE_ID, Off2OnServiceImplTest.ITEM_SKU);
    assertEquals(result, true);
    verify(this.off2OnHelperService).changeOff2OnChannelActiveByItemSku(Off2OnServiceImplTest.STORE_ID,
        Off2OnServiceImplTest.ITEM_SKU, true);
  }

  @Test
  public void activateOff2OnChannelByMerchantCodeTest() {
    ArrayList<String> expectedResult = new ArrayList<String>();
    when(this.off2OnHelperService.changeOff2OnChannelActiveByMerchantCode(Off2OnServiceImplTest.STORE_ID,
        Off2OnServiceImplTest.MERCHANT_CODE, true)).thenReturn(expectedResult);
    List<String> result = this.service.activateOff2OnChannelByMerchantCode(Off2OnServiceImplTest.STORE_ID,
        Off2OnServiceImplTest.MERCHANT_CODE);
    verify(this.off2OnHelperService).changeOff2OnChannelActiveByMerchantCode(Off2OnServiceImplTest.STORE_ID,
        Off2OnServiceImplTest.MERCHANT_CODE, true);
    assertEquals(expectedResult, result);
  }

  @Test
  public void activateOff2OnChannelByProductSkuListTest() {
    ArrayList<String> expectedResult = new ArrayList<String>();
    expectedResult.add(Off2OnServiceImplTest.PRODUCT_SKU);
    when(this.off2OnHelperService.changeOff2OnChannelActiveByProductSku(Off2OnServiceImplTest.STORE_ID, expectedResult,
        true)).thenReturn(expectedResult);
    List<String> result =
        this.service.activateOff2OnChannelByProductSku(Off2OnServiceImplTest.STORE_ID, expectedResult);
    verify(this.off2OnHelperService).changeOff2OnChannelActiveByProductSku(Off2OnServiceImplTest.STORE_ID,
        expectedResult, true);
    assertEquals(expectedResult, result);
  }

  @Test
  public void activateOff2OnChannelByProductSkuTest() {
    when(this.off2OnHelperService.changeOff2OnChannelActiveByProductSku(Off2OnServiceImplTest.STORE_ID,
        Off2OnServiceImplTest.PRODUCT_SKU, true)).thenReturn(true);
    boolean result = this.service.activateOff2OnChannelByProductSku(Off2OnServiceImplTest.STORE_ID,
        Off2OnServiceImplTest.PRODUCT_SKU);
    verify(this.off2OnHelperService).changeOff2OnChannelActiveByProductSku(Off2OnServiceImplTest.STORE_ID,
        Off2OnServiceImplTest.PRODUCT_SKU, true);
    assertEquals(true, result);
  }

  @Test
  public void deactivateOff2OnChannelByItemSkuListTest() {
    List<String> itemSkus = new ArrayList<String>();
    itemSkus.add(Off2OnServiceImplTest.ITEM_SKU);
    when(this.off2OnHelperService.changeOff2OnChannelActiveByItemSku(Off2OnServiceImplTest.STORE_ID, itemSkus,
        false)).thenReturn(itemSkus);
    List<String> result = this.service.deactivateOff2OnChannelByItemSku(Off2OnServiceImplTest.STORE_ID, itemSkus);
    assertEquals(result, itemSkus);
    verify(this.off2OnHelperService).changeOff2OnChannelActiveByItemSku(Off2OnServiceImplTest.STORE_ID, itemSkus,
        false);
  }

  @Test
  public void deactivateOff2OnChannelByItemSkuTest() {
    when(this.off2OnHelperService.changeOff2OnChannelActiveByItemSku(Off2OnServiceImplTest.STORE_ID,
        Off2OnServiceImplTest.ITEM_SKU, false)).thenReturn(true);
    boolean result =
        this.service.deactivateOff2OnChannelByItemSku(Off2OnServiceImplTest.STORE_ID, Off2OnServiceImplTest.ITEM_SKU);
    verify(this.off2OnHelperService).changeOff2OnChannelActiveByItemSku(Off2OnServiceImplTest.STORE_ID,
        Off2OnServiceImplTest.ITEM_SKU, false);
    assertEquals(result, true);
  }

  @Test
  public void deactivateOff2OnChannelByMerchantCodeTest() {
    ArrayList<String> expectedResult = new ArrayList<String>();
    when(this.off2OnHelperService.changeOff2OnChannelActiveByMerchantCode(Off2OnServiceImplTest.STORE_ID,
        Off2OnServiceImplTest.MERCHANT_CODE, false)).thenReturn(expectedResult);
    List<String> result = this.service.deactivateOff2OnChannelByMerchantCode(Off2OnServiceImplTest.STORE_ID,
        Off2OnServiceImplTest.MERCHANT_CODE);
    verify(this.off2OnHelperService).changeOff2OnChannelActiveByMerchantCode(Off2OnServiceImplTest.STORE_ID,
        Off2OnServiceImplTest.MERCHANT_CODE, false);
    assertEquals(expectedResult, result);
  }

  @Test
  public void deactivateOff2OnChannelByProductSkuListTest() {
    ArrayList<String> expectedResult = new ArrayList<String>();
    expectedResult.add(Off2OnServiceImplTest.PRODUCT_SKU);
    when(this.off2OnHelperService.changeOff2OnChannelActiveByProductSku(Off2OnServiceImplTest.STORE_ID, expectedResult,
        false)).thenReturn(expectedResult);
    List<String> result =
        this.service.deactivateOff2OnChannelByProductSku(Off2OnServiceImplTest.STORE_ID, expectedResult);
    verify(this.off2OnHelperService).changeOff2OnChannelActiveByProductSku(Off2OnServiceImplTest.STORE_ID,
        expectedResult, false);
    assertEquals(expectedResult, result);
  }

  @Test
  public void deactivateOff2OnChannelByProductSkuTest() {
    when(this.off2OnHelperService.changeOff2OnChannelActiveByProductSku(Off2OnServiceImplTest.STORE_ID,
        Off2OnServiceImplTest.PRODUCT_SKU, false)).thenReturn(true);
    boolean result = this.service.deactivateOff2OnChannelByProductSku(Off2OnServiceImplTest.STORE_ID,
        Off2OnServiceImplTest.PRODUCT_SKU);
    verify(this.off2OnHelperService).changeOff2OnChannelActiveByProductSku(Off2OnServiceImplTest.STORE_ID,
        Off2OnServiceImplTest.PRODUCT_SKU, false);
    assertEquals(true, result);
  }

  @Test
  public void getProductPriceForOff2OnBlankChannelTest() throws Exception {
    List<String> itemSkuList = new ArrayList<String>();
    itemSkuList.add(Off2OnServiceImplTest.ITEM_SKU);

    List<Item> items = new ArrayList<Item>();
    Item item = new Item(Off2OnServiceImplTest.ITEM_SKU, Off2OnServiceImplTest.PRODUCT_SKU);
    Set<Price> prices = new HashSet<Price>();
    Price price =
        new Price(Off2OnServiceImplTest.CURRENCY, Off2OnServiceImplTest.OFFER_PRICE, Off2OnServiceImplTest.LIST_PRICE,
            Off2OnServiceImplTest.CHANNEL, null, null);
    prices.add(price);
    item.setPrice(prices);
    item.setOff2OnChannelActive(Off2OnServiceImplTest.OFF2_ON_CHANNEL_ACTIVE);
    items.add(item);

    ItemPickupPoint itemPickupPoint =
        ItemPickupPoint.builder().itemSku(item.getItemSku()).pickupPointCode(item.getPickupPointCode())
            .itemViewConfig(item.getItemViewConfigs()).price(item.getPrice()).build();

    List<Off2OnPriceVO> expectedResult = new ArrayList<Off2OnPriceVO>();
    expectedResult.add(new Off2OnPriceVO(Off2OnServiceImplTest.ITEM_SKU, Off2OnServiceImplTest.OFFER_PRICE,
        Off2OnServiceImplTest.LIST_PRICE, Off2OnServiceImplTest.OFF2_ON_CHANNEL_ACTIVE));

    when(this.itemService.getItemPriceAndOff2OnChannelActive(Off2OnServiceImplTest.STORE_ID, itemSkuList)).thenReturn(
        items);
    when(itemPickupPointService.findByItemSkusAndDelivery(STORE_ID, itemSkuList, true)).thenReturn(
        Arrays.asList(itemPickupPoint));
    when(this.productHelperService.getRelevantItemPrice(prices, Off2OnServiceImplTest.CHANNEL)).thenReturn(price);
    when(this.channelService.getDefaultChannel()).thenReturn(Off2OnServiceImplTest.CHANNEL);
    List<Off2OnPriceVO> result =
        this.service.getProductPriceForOff2On(Off2OnServiceImplTest.STORE_ID, itemSkuList, Off2OnServiceImplTest.BLANK);
    assertEquals(result, expectedResult);
    verify(this.itemService).getItemPriceAndOff2OnChannelActive(Off2OnServiceImplTest.STORE_ID, itemSkuList);
    verify(this.productHelperService).getRelevantItemPrice(prices, Off2OnServiceImplTest.CHANNEL);
    verify(this.channelService).getDefaultChannel();
    verify(itemPickupPointService).findByItemSkusAndDelivery(STORE_ID, itemSkuList, true);
  }

  @Test
  public void getProductPriceForOff2OnV2Test() throws Exception {
    List<ItemPickupPointRequest> itemPickupPointRequestList =
        Arrays.asList(new ItemPickupPointRequest(ITEM_SKU, PICKUP_POINT_CODE));
    List<ItemPickupPoint> itemPickupPointList =
        Arrays.asList(ItemPickupPoint.builder().itemSku(ITEM_SKU).pickupPointCode(PICKUP_POINT_CODE).build());
    Map<String, Item> itemMap = ImmutableMap.of(ITEM_SKU, new Item());

    when(itemPickupPointService.fetchItemPickupPointsByItemSkuAndPickupPointCode(STORE_ID,
        itemPickupPointRequestList, false)).thenReturn(itemPickupPointList);
    when(itemService.fetchItemMapByItemSkus(STORE_ID, ImmutableSet.of(ITEM_SKU))).thenReturn(itemMap);
    when(productHelperService.getRelevantItemPrice(Mockito.isNull(), Mockito.anyString())).thenReturn(
        new Price());

    service.getProductPriceForOff2OnV2(STORE_ID, itemPickupPointRequestList, CHANNEL, false);

    verify(itemPickupPointService).fetchItemPickupPointsByItemSkuAndPickupPointCode(STORE_ID,
        itemPickupPointRequestList, false);
    verify(itemService).fetchItemMapByItemSkus(STORE_ID, ImmutableSet.of(ITEM_SKU));
    verify(productHelperService).getRelevantItemPrice(Mockito.isNull(), Mockito.anyString());
  }

  @Test
  public void getProductPriceForOff2OnV2WithAllStorePromoTest() throws Exception {
    List<ItemPickupPointRequest> itemPickupPointRequestList =
        Arrays.asList(new ItemPickupPointRequest(ITEM_SKU, PICKUP_POINT_CODE));
    List<ItemPickupPoint> itemPickupPointList =
        Arrays.asList(ItemPickupPoint.builder().itemSku(ITEM_SKU).pickupPointCode(PICKUP_POINT_CODE).build());
    Map<String, Item> itemMap = ImmutableMap.of(ITEM_SKU, new Item());

    when(itemPickupPointService.fetchItemPickupPointsByItemSkuAndPickupPointCode(STORE_ID,
        itemPickupPointRequestList, false)).thenReturn(itemPickupPointList);
    when(itemService.fetchItemMapByItemSkus(STORE_ID, ImmutableSet.of(ITEM_SKU))).thenReturn(itemMap);
    when(productHelperService.getRelevantItemPrice(Mockito.isNull(), Mockito.anyString())).thenReturn(
        new Price());
    when(businessPartnerPromoService.findByStoreIdAndBusinessPartnerList(STORE_ID,
        itemPickupPointList.stream().map(ItemPickupPoint::getMerchantCode).distinct()
            .collect(Collectors.toList()))).thenReturn(new ArrayList<>());
    service.getProductPriceForOff2OnV2(STORE_ID, itemPickupPointRequestList, CHANNEL, true);

    verify(itemPickupPointService).fetchItemPickupPointsByItemSkuAndPickupPointCode(STORE_ID,
        itemPickupPointRequestList, false);
    verify(itemService).fetchItemMapByItemSkus(STORE_ID, ImmutableSet.of(ITEM_SKU));
    verify(businessPartnerPromoService).findByStoreIdAndBusinessPartnerList(STORE_ID,
        itemPickupPointList.stream().map(ItemPickupPoint::getMerchantCode).distinct()
            .collect(Collectors.toList()));
    verify(productHelperService).getRelevantItemPrice(Mockito.isNull(), Mockito.anyString());
  }

  @Test
  public void getProductPriceForOff2OnV2DefaultChannelTest() throws Exception {
    List<ItemPickupPointRequest> itemPickupPointRequestList =
        Arrays.asList(new ItemPickupPointRequest(ITEM_SKU, PICKUP_POINT_CODE));
    List<ItemPickupPoint> itemPickupPointList =
        Arrays.asList(ItemPickupPoint.builder().itemSku(ITEM_SKU).pickupPointCode(PICKUP_POINT_CODE).build());
    Map<String, Item> itemMap = ImmutableMap.of(ITEM_SKU, new Item());

    when(itemPickupPointService.fetchItemPickupPointsByItemSkuAndPickupPointCode(STORE_ID,
        itemPickupPointRequestList, false)).thenReturn(itemPickupPointList);
    when(itemService.fetchItemMapByItemSkus(STORE_ID, ImmutableSet.of(ITEM_SKU))).thenReturn(itemMap);
    when(productHelperService.getRelevantItemPrice(Mockito.isNull(), Mockito.anyString())).thenReturn(
        new Price());
    when(channelService.getDefaultChannel()).thenReturn(CHANNEL);

    service.getProductPriceForOff2OnV2(STORE_ID, itemPickupPointRequestList, StringUtils.EMPTY, false);

    verify(itemPickupPointService).fetchItemPickupPointsByItemSkuAndPickupPointCode(STORE_ID,
        itemPickupPointRequestList, false);
    verify(itemService).fetchItemMapByItemSkus(STORE_ID, ImmutableSet.of(ITEM_SKU));
    verify(productHelperService).getRelevantItemPrice(Mockito.isNull(), Mockito.anyString());
    verify(channelService).getDefaultChannel();
  }

  @Test
  public void getProductPriceForOff2OnV2NullStoreIdTest() throws Exception {
    List<ItemPickupPointRequest> itemPickupPointRequestList =
        Arrays.asList(new ItemPickupPointRequest(ITEM_SKU, PICKUP_POINT_CODE));
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> service.getProductPriceForOff2OnV2(StringUtils.EMPTY, itemPickupPointRequestList, StringUtils.EMPTY,
        OFF2_ON_CHANNEL_ACTIVE));
  }

  @Test
  public void getProductPriceForOff2OnV2NullRequestTest() throws Exception {
    List<ItemPickupPointRequest> itemPickupPointRequestList =
        Arrays.asList(new ItemPickupPointRequest(ITEM_SKU, PICKUP_POINT_CODE));
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> service.getProductPriceForOff2OnV2(StringUtils.EMPTY, null, StringUtils.EMPTY,
        OFF2_ON_CHANNEL_ACTIVE));
  }

  @BeforeEach
  public void setUp() throws Exception {
    openMocks(this);
  }

  @AfterEach
  public void tearDown() throws Exception {
    verifyNoMoreInteractions(this.off2OnHelperService);
    verifyNoMoreInteractions(this.productHelperService);
    verifyNoMoreInteractions(this.channelService);
    verifyNoMoreInteractions(this.itemService);
    verifyNoMoreInteractions(this.businessPartnerPromoService);
  }
}
