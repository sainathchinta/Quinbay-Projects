package com.gdn.x.product.service.impl;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.MockitoAnnotations.openMocks;

import java.time.Duration;
import java.time.Instant;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collections;
import java.util.Date;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.springframework.test.util.ReflectionTestUtils;

import com.gdn.x.product.enums.Constants;
import com.gdn.x.product.model.entity.DiscountPrice;
import com.gdn.x.product.model.entity.ItemBuyableSchedule;
import com.gdn.x.product.model.entity.ItemDiscoverableSchedule;
import com.gdn.x.product.model.entity.ItemPickupPoint;
import com.gdn.x.product.model.entity.ItemViewConfig;
import com.gdn.x.product.model.entity.Price;
import com.gdn.x.product.rest.web.model.response.AutoCreatePickupPointResponse;
import com.gdn.x.product.service.api.ChannelService;
import com.gdn.x.product.service.api.ObjectConverterService;
import com.gdn.x.product.service.api.PriceHistoryService;

public class ItemPickupPointHelperServiceImplTest {

  @InjectMocks
  private ItemPickupPointHelperServiceImpl itemPickupPointHelperService;

  @Mock
  private PriceHistoryService priceHistoryService;

  @Mock
  private ObjectConverterService objectConverterService;

  @Mock
  private ChannelService channelService;
  
  private Set<ItemViewConfig> itemViewConfigs;
  private Set<Price> prices;
  private static final String DEFAULT_CHANNEL = "default-channel";
  private static final String OTHER_CHANNEL = "other-channel";
  private static final String USERNAME = "username";
  private static final String CHANNEL = "channel";
  private static final String CHANNEL_2 = "channel_2";
  private static final String ITEM_SKU = "itemSku";
  private static final String DEFAULT = "DEFAULT";
  private ItemPickupPoint itemPickupPoint;
  private Set<ItemViewConfig> itemViewConfigSet = new HashSet<>();

  private static final ItemViewConfig DEFAULT_CHANNEL_OBJ =
    new ItemViewConfig(true, true, ItemPickupPointHelperServiceImplTest.DEFAULT_CHANNEL, null,
      null);
  private static final ItemViewConfig OTHER_CHANNEL_OBJ =
    new ItemViewConfig(true, true, ItemPickupPointHelperServiceImplTest.OTHER_CHANNEL, null, null);


  @BeforeEach
  public void setUp() throws Exception {
    openMocks(this);
    this.itemViewConfigs = new HashSet<ItemViewConfig>();
    this.itemViewConfigs.add(ItemPickupPointHelperServiceImplTest.DEFAULT_CHANNEL_OBJ);
    this.itemViewConfigs.add(ItemPickupPointHelperServiceImplTest.OTHER_CHANNEL_OBJ);
    prices = new HashSet<>();
    Price price = new Price();
    price.setOfferPrice(800);
    price.setListPrice(1000);
    price.setChannel(ItemPickupPointHelperServiceImplTest.DEFAULT_CHANNEL);
    prices.add(price);

    ItemViewConfig itemViewConfig = new ItemViewConfig();
    itemViewConfig.setChannel(Constants.DEFAULT);
    itemViewConfig.setDiscoverable(true);
    itemViewConfig.setBuyable(true);

    ItemBuyableSchedule itemBuyableSchedule = new ItemBuyableSchedule();
    itemBuyableSchedule.setBuyable(true);
    itemBuyableSchedule
      .setStartDateTime(new Date(Calendar.getInstance().getTimeInMillis() - 100000));
    itemBuyableSchedule.setEndDateTime(new Date(Calendar.getInstance().getTimeInMillis() + 100000));

    ItemDiscoverableSchedule itemDiscoverableSchedule = new ItemDiscoverableSchedule();
    itemDiscoverableSchedule.setDiscoverable(true);

    itemViewConfig.setItemBuyableSchedules(itemBuyableSchedule);
    itemViewConfig.setItemDiscoverableSchedules(itemDiscoverableSchedule);
    itemViewConfig.setChannel(Constants.DEFAULT);

    itemPickupPoint = new ItemPickupPoint();
    itemViewConfigSet.add(itemViewConfig);
    itemPickupPoint.setItemSku(ITEM_SKU);
    itemPickupPoint.setItemViewConfig(itemViewConfigSet);
  }

  @AfterEach
  public void tearDown() {
    Mockito.verifyNoMoreInteractions(priceHistoryService);
    Mockito.verifyNoMoreInteractions(objectConverterService);
  }

  @Test
  public void isItemPriceChangeTest() {
    ItemPickupPoint itemPickupPoint = new ItemPickupPoint();
    Set<Price> itemPrices = new HashSet<>();
    Price price = new Price();
    price.setOfferPrice(700);
    price.setListPrice(1000);
    price.setChannel(ItemPickupPointHelperServiceImplTest.DEFAULT_CHANNEL);
    itemPrices.add(price);
    itemPickupPoint.setPrice(itemPrices);
    boolean result =
      this.itemPickupPointHelperService.isItemPickupPointPriceChange(itemPickupPoint, prices);
    assertTrue(result);
  }

  @Test
  public void isItemPriceChange_whenPriceNotChangeTest() {
    ItemPickupPoint itemPickupPoint = new ItemPickupPoint();
    Set<Price> itemPrices = new HashSet<>();
    Price price = new Price();
    price.setOfferPrice(800);
    price.setListPrice(1000);
    price.setChannel(ItemPickupPointHelperServiceImplTest.DEFAULT_CHANNEL);
    itemPrices.add(price);
    itemPickupPoint.setPrice(itemPrices);
    boolean result =
      this.itemPickupPointHelperService.isItemPickupPointPriceChange(itemPickupPoint, prices);
    assertFalse(result);
  }

  @Test
  public void isItemPriceChange_WhenMerchantPromoDiscountTrueAndUpcomingPromoNoPriceChangeTest() {
    ItemPickupPoint itemPickupPoint = new ItemPickupPoint();
    itemPickupPoint.setMerchantPromoDiscount(true);
    Set<Price> itemPrices = new HashSet<>();
    Price existingPrice = new Price();
    existingPrice.setOfferPrice(800);
    existingPrice.setListPrice(1000);
    existingPrice.setChannel(ItemPickupPointHelperServiceImplTest.DEFAULT_CHANNEL);
    itemPrices.add(existingPrice);
    itemPickupPoint.setPrice(itemPrices);
    boolean result =
      this.itemPickupPointHelperService.isItemPickupPointPriceChange(itemPickupPoint, prices);
    Assertions.assertFalse(result);
  }

  @Test
  public void isItemPriceChange_WhenMerchantPromoDiscountTrueAndUpcomingPromoPriceChangedTest() {
    ItemPickupPoint itemPickupPoint = new ItemPickupPoint();
    itemPickupPoint.setMerchantPromoDiscount(true);
    Set<Price> itemPrices = new HashSet<>();
    Price existingPrice = new Price();
    existingPrice.setOfferPrice(700);
    existingPrice.setListPrice(1000);
    existingPrice.setChannel(ItemPickupPointHelperServiceImplTest.DEFAULT_CHANNEL);
    itemPrices.add(existingPrice);
    itemPickupPoint.setPrice(itemPrices);
    boolean result =
      this.itemPickupPointHelperService.isItemPickupPointPriceChange(itemPickupPoint, prices);
    Assertions.assertTrue(result);
  }

  @Test
  public void isItemPriceChange_WhenMerchantPromoDiscountTrueAndPriceChangedTest() {
    ItemPickupPoint itemPickupPoint = new ItemPickupPoint();
    itemPickupPoint.setMerchantPromoDiscount(true);
    Set<Price> itemPrices = new HashSet<>();
    Price existingPrice = new Price();
    existingPrice.setOfferPrice(700);
    existingPrice.setListPrice(1000);
    existingPrice.setMerchantPromoDiscountPrice(new DiscountPrice());
    existingPrice.getMerchantPromoDiscountPrice().setDiscountPrice(900);
    existingPrice.setChannel(ItemPickupPointHelperServiceImplTest.DEFAULT_CHANNEL);
    itemPrices.add(existingPrice);
    itemPickupPoint.setPrice(itemPrices);
    boolean result =
      this.itemPickupPointHelperService.isItemPickupPointPriceChange(itemPickupPoint, prices);
    Assertions.assertTrue(result);
  }

  @Test
  public void isItemPriceChange_whenChannelNotMatchTest() {
    ItemPickupPoint itemPickupPoint = new ItemPickupPoint();
    Set<Price> itemPrices = new HashSet<>();
    Price price = new Price();
    price.setOfferPrice(700);
    price.setListPrice(1000);
    price.setChannel(ItemPickupPointHelperServiceImplTest.OTHER_CHANNEL);
    itemPrices.add(price);
    itemPickupPoint.setPrice(itemPrices);
    boolean result =
      this.itemPickupPointHelperService.isItemPickupPointPriceChange(itemPickupPoint, prices);
    Assertions.assertFalse(result);
  }

  @Test
  public void isItemPriceChange_whenMerchantPromoChannelNotMatchTest() {
    ItemPickupPoint itemPickupPoint = new ItemPickupPoint();
    itemPickupPoint.setMerchantPromoDiscount(true);
    Set<Price> itemPrices = new HashSet<>();
    Price price = new Price();
    price.setOfferPrice(700);
    price.setListPrice(1000);
    price.setChannel(ItemPickupPointHelperServiceImplTest.OTHER_CHANNEL);
    itemPrices.add(price);
    itemPickupPoint.setPrice(itemPrices);
    boolean result =
      this.itemPickupPointHelperService.isItemPickupPointPriceChange(itemPickupPoint, prices);
    Assertions.assertFalse(result);
  }

  @Test
  public void isItemPriceChange_whenListPriceChangeTest() {
    ItemPickupPoint itemPickupPoint = new ItemPickupPoint();
    Set<Price> itemPrices = new HashSet<>();
    Price price = new Price();
    price.setOfferPrice(700);
    price.setListPrice(900);
    price.setChannel(ItemPickupPointHelperServiceImplTest.DEFAULT_CHANNEL);
    itemPrices.add(price);
    itemPickupPoint.setPrice(itemPrices);
    boolean result =
      this.itemPickupPointHelperService.isItemPickupPointPriceChange(itemPickupPoint, prices);
    Assertions.assertTrue(result);
  }

  @Test
  public void isItemPriceChange_whenEmptyPriceSetTest() {
    ItemPickupPoint itemPickupPoint = new ItemPickupPoint();
    Set<Price> itemPrices = new HashSet<>();
    Price price = new Price();
    price.setOfferPrice(700);
    price.setListPrice(900);
    price.setChannel(ItemPickupPointHelperServiceImplTest.DEFAULT_CHANNEL);
    itemPrices.add(price);
    itemPickupPoint.setPrice(itemPrices);
    boolean result = this.itemPickupPointHelperService
      .isItemPickupPointPriceChange(itemPickupPoint, new HashSet<Price>());
    Assertions.assertFalse(result);
  }

  @Test
  public void isItemPickupPointPriceChangeForOriginalSellingPriceTest() {
    ItemPickupPoint itemPickupPoint = new ItemPickupPoint();
    Set<Price> itemPrices = new HashSet<>();
    Price price = new Price();
    price.setOfferPrice(700);
    price.setListPrice(1000);
    price.setChannel(ItemPickupPointHelperServiceImplTest.DEFAULT_CHANNEL);
    itemPrices.add(price);
    itemPickupPoint.setPrice(itemPrices);
    boolean result =
        this.itemPickupPointHelperService.isItemPickupPointPriceChangeForOriginalSellingPrice(itemPickupPoint, prices);
    assertTrue(result);
  }

  @Test
  public void isItemPickupPointPriceChangeForOriginalSellingPrice_whenPriceNotChangeTest() {
    ItemPickupPoint itemPickupPoint = new ItemPickupPoint();
    Set<Price> itemPrices = new HashSet<>();
    Price price = new Price();
    price.setOfferPrice(800);
    price.setListPrice(1000);
    price.setChannel(ItemPickupPointHelperServiceImplTest.DEFAULT_CHANNEL);
    itemPrices.add(price);
    itemPickupPoint.setPrice(itemPrices);
    boolean result =
        this.itemPickupPointHelperService.isItemPickupPointPriceChangeForOriginalSellingPrice(itemPickupPoint, prices);
    assertFalse(result);
  }

  @Test
  public void isItemPickupPointPriceChangeForOriginalSellingPrice_WhenMerchantPromoDiscountTrueAndUpcomingPromoNoPriceChangeTest() {
    ItemPickupPoint itemPickupPoint = new ItemPickupPoint();
    itemPickupPoint.setMerchantPromoDiscount(true);
    Set<Price> itemPrices = new HashSet<>();
    Price existingPrice = new Price();
    existingPrice.setOfferPrice(800);
    existingPrice.setListPrice(1000);
    existingPrice.setChannel(ItemPickupPointHelperServiceImplTest.DEFAULT_CHANNEL);
    itemPrices.add(existingPrice);
    itemPickupPoint.setPrice(itemPrices);
    boolean result =
        this.itemPickupPointHelperService.isItemPickupPointPriceChangeForOriginalSellingPrice(itemPickupPoint, prices);
    Assertions.assertFalse(result);
  }

  @Test
  public void isItemPickupPointPriceChangeForOriginalSellingPrice_whenChannelNotMatchTest() {
    ItemPickupPoint itemPickupPoint = new ItemPickupPoint();
    Set<Price> itemPrices = new HashSet<>();
    Price price = new Price();
    price.setOfferPrice(700);
    price.setListPrice(1000);
    price.setChannel(ItemPickupPointHelperServiceImplTest.OTHER_CHANNEL);
    itemPrices.add(price);
    itemPickupPoint.setPrice(itemPrices);
    boolean result =
        this.itemPickupPointHelperService.isItemPickupPointPriceChangeForOriginalSellingPrice(itemPickupPoint, prices);
    Assertions.assertFalse(result);
  }

  @Test
  public void isItemPickupPointPriceChangeForOriginalSellingPrice_whenMerchantPromoChannelNotMatchTest() {
    ItemPickupPoint itemPickupPoint = new ItemPickupPoint();
    itemPickupPoint.setMerchantPromoDiscount(true);
    Set<Price> itemPrices = new HashSet<>();
    Price price = new Price();
    price.setOfferPrice(700);
    price.setListPrice(1000);
    price.setChannel(ItemPickupPointHelperServiceImplTest.OTHER_CHANNEL);
    itemPrices.add(price);
    itemPickupPoint.setPrice(itemPrices);
    boolean result =
        this.itemPickupPointHelperService.isItemPickupPointPriceChangeForOriginalSellingPrice(itemPickupPoint, prices);
    Assertions.assertFalse(result);
  }

  @Test
  public void isItemPickupPointPriceChangeForOriginalSellingPrice_whenListPriceChangeTest() {
    ItemPickupPoint itemPickupPoint = new ItemPickupPoint();
    Set<Price> itemPrices = new HashSet<>();
    Price price = new Price();
    price.setOfferPrice(700);
    price.setListPrice(900);
    price.setChannel(ItemPickupPointHelperServiceImplTest.DEFAULT_CHANNEL);
    itemPrices.add(price);
    itemPickupPoint.setPrice(itemPrices);
    boolean result =
        this.itemPickupPointHelperService.isItemPickupPointPriceChangeForOriginalSellingPrice(itemPickupPoint, prices);
    Assertions.assertTrue(result);
  }

  @Test
  public void isItemPickupPointPriceChangeForOriginalSellingPriceWhenEmptyPriceSetTest() {
    ItemPickupPoint itemPickupPoint = new ItemPickupPoint();
    Set<Price> itemPrices = new HashSet<>();
    Price price = new Price();
    price.setOfferPrice(700);
    price.setListPrice(900);
    price.setChannel(ItemPickupPointHelperServiceImplTest.DEFAULT_CHANNEL);
    itemPrices.add(price);
    itemPickupPoint.setPrice(itemPrices);
    boolean result = this.itemPickupPointHelperService
        .isItemPickupPointPriceChangeForOriginalSellingPrice(itemPickupPoint, new HashSet<Price>());
    Assertions.assertFalse(result);
  }

  @Test
  public void setItemPriceByChannelTest() {
    ItemPickupPoint itemPickupPoint = new ItemPickupPoint();
    Set<Price> itemPrices = new HashSet<>();
    Price price = new Price();
    price.setOfferPrice(700);
    price.setListPrice(900);
    price.setChannel(ItemPickupPointHelperServiceImplTest.DEFAULT_CHANNEL);
    DiscountPrice discountPrice = new DiscountPrice();
    discountPrice.setDiscountPrice(100);
    price.setListOfDiscountPrices(Collections.singletonList(discountPrice));
    itemPrices.add(price);
    itemPickupPoint.setPrice(itemPrices);
    Set<Price> updatedPrices = new HashSet<>();
    Price updatedPrice = new Price();
    updatedPrice.setChannel(ItemPickupPointHelperServiceImplTest.DEFAULT_CHANNEL);
    updatedPrice.setListPrice(800);
    updatedPrices.add(updatedPrice);
    Mockito.when(channelService.getDefaultChannel()).thenReturn(ItemPickupPointHelperServiceImplTest.DEFAULT_CHANNEL);
    this.itemPickupPointHelperService.setItemPriceByChannel(itemPickupPoint, updatedPrices,
      ItemPickupPointHelperServiceImplTest.USERNAME);
    Mockito.verify(this.priceHistoryService).savePriceHistory(Mockito.isNull());
    Mockito.verify(this.objectConverterService)
      .convertToPriceHistory(price, itemPickupPoint.getItemSku());
    Assertions.assertEquals(discountPrice.getDiscountPrice(),
      updatedPrice.getListOfDiscountPrices().get(0).getDiscountPrice(), 0.0);
  }

  @Test
  public void setItemPriceByDifferentChannelTest() {
    ItemPickupPoint itemPickupPoint = new ItemPickupPoint();
    Set<Price> itemPrices = new HashSet<>();
    Price price = new Price();
    price.setOfferPrice(700);
    price.setListPrice(900);
    price.setChannel(ItemPickupPointHelperServiceImplTest.OTHER_CHANNEL);
    DiscountPrice discountPrice = new DiscountPrice();
    discountPrice.setDiscountPrice(100);
    price.setListOfDiscountPrices(Collections.singletonList(discountPrice));
    itemPrices.add(price);
    itemPickupPoint.setPrice(itemPrices);
    Set<Price> updatedPrices = new HashSet<>();
    Price updatedPrice = new Price();
    updatedPrice.setChannel(ItemPickupPointHelperServiceImplTest.DEFAULT_CHANNEL);
    updatedPrice.setListPrice(800);
    updatedPrices.add(updatedPrice);
    this.itemPickupPointHelperService.setItemPriceByChannel(itemPickupPoint, updatedPrices,
        ItemPickupPointHelperServiceImplTest.USERNAME);
  }

  @Test
  public void setItemPriceSameListPriceTest() {
    ItemPickupPoint itemPickupPoint = new ItemPickupPoint();
    Set<Price> itemPrices = new HashSet<>();
    Price price = new Price();
    price.setOfferPrice(700);
    price.setListPrice(800);
    price.setChannel(ItemPickupPointHelperServiceImplTest.DEFAULT_CHANNEL);
    DiscountPrice discountPrice = new DiscountPrice();
    discountPrice.setDiscountPrice(100);
    price.setListOfDiscountPrices(Collections.singletonList(discountPrice));
    itemPrices.add(price);
    itemPickupPoint.setPrice(itemPrices);
    Set<Price> updatedPrices = new HashSet<>();
    Price updatedPrice = new Price();
    updatedPrice.setChannel(ItemPickupPointHelperServiceImplTest.DEFAULT_CHANNEL);
    updatedPrice.setListPrice(800);
    updatedPrices.add(updatedPrice);
    Mockito.when(channelService.getDefaultChannel()).thenReturn(ItemPickupPointHelperServiceImplTest.DEFAULT_CHANNEL);
    this.itemPickupPointHelperService.setItemPriceByChannel(itemPickupPoint, updatedPrices,
      ItemPickupPointHelperServiceImplTest.USERNAME);
    Mockito.verify(this.priceHistoryService).savePriceHistory(Mockito.isNull());
    Mockito.verify(this.objectConverterService)
      .convertToPriceHistory(price, itemPickupPoint.getItemSku());
    Assertions.assertEquals(discountPrice.getDiscountPrice(),
      updatedPrice.getListOfDiscountPrices().get(0).getDiscountPrice(), 0.0);
  }

  @Test
  public void isItemViewConfigChangeForExistingChannelChangeTest() {
    Set<ItemViewConfig> newItemViewConfigSet = new HashSet<>();
    ItemViewConfig newItemViewConfig = new ItemViewConfig();
    newItemViewConfig.setChannel(DEFAULT);
    newItemViewConfigSet.add(newItemViewConfig);

    boolean result = this.itemPickupPointHelperService
      .isItemViewConfigChangeForExistingChannel(itemPickupPoint, newItemViewConfigSet);
    assertTrue(result);
  }

  @Test
  public void isItemViewConfigChangeForExistingChannelChange_whenDiscoverableFlagChangeTest() {
    Set<ItemViewConfig> newItemViewConfigSet = new HashSet<>();
    ItemViewConfig newItemViewConfig = new ItemViewConfig();
    newItemViewConfig.setBuyable(true);
    newItemViewConfig.setDiscoverable(true);
    newItemViewConfig.setChannel(DEFAULT);
    newItemViewConfigSet.add(newItemViewConfig);
    boolean result = this.itemPickupPointHelperService
      .isItemViewConfigChangeForExistingChannel(itemPickupPoint, newItemViewConfigSet);
    assertTrue(result);
  }

  @Test
  public void isItemViewConfigChangeForExistingChannelChange_whenNoChangeTest() {

    Date date = new Date();
    Set<ItemViewConfig> newItemViewConfigSet = new HashSet<>();
    ItemViewConfig newItemViewConfig = new ItemViewConfig();
    newItemViewConfig.setChannel(DEFAULT);
    newItemViewConfig.setBuyable(true);
    newItemViewConfigSet.add(newItemViewConfig);
    ItemDiscoverableSchedule itemDiscoverableSchedule =
      new ItemDiscoverableSchedule(false, date, date);
    newItemViewConfig.setItemDiscoverableScheduleIfNotNull(itemDiscoverableSchedule);
    itemPickupPoint.getItemViewConfig().iterator().next()
      .setItemDiscoverableScheduleIfNotNull(itemDiscoverableSchedule);
    boolean result = this.itemPickupPointHelperService
      .isItemViewConfigChangeForExistingChannel(itemPickupPoint, newItemViewConfigSet);
    assertTrue(result);
  }

  @Test
  public void isItemViewConfigChangeForExistingChannelChange_whenChangeTest() {

    Date date = new Date();
    Set<ItemViewConfig> newItemViewConfigSet = new HashSet<>();
    ItemViewConfig newItemViewConfig = new ItemViewConfig();
    newItemViewConfig.setChannel(DEFAULT);
    newItemViewConfig.setBuyable(true);
    newItemViewConfigSet.add(newItemViewConfig);
    ItemDiscoverableSchedule newItemDiscoverableSchedule =
      new ItemDiscoverableSchedule(false, date, date);
    newItemViewConfig.setItemDiscoverableScheduleIfNotNull(newItemDiscoverableSchedule);
    ItemDiscoverableSchedule itemDiscoverableSchedule =
      new ItemDiscoverableSchedule(true, date, date);
    itemPickupPoint.getItemViewConfig().iterator().next()
      .setItemDiscoverableScheduleIfNotNull(itemDiscoverableSchedule);
    boolean result = this.itemPickupPointHelperService
      .isItemViewConfigChangeForExistingChannel(itemPickupPoint, newItemViewConfigSet);
    assertTrue(result);
  }

  @Test
  public void isItemViewConfigChangeForExistingChannelFalseTest() {
    Date date = new Date();
    Set<ItemViewConfig> newItemViewConfigSet = new HashSet<>();
    ItemViewConfig newItemViewConfig = new ItemViewConfig();
    newItemViewConfig.setBuyable(true);
    newItemViewConfig.setDiscoverable(true);
    newItemViewConfig.setChannel(DEFAULT);
    ItemDiscoverableSchedule itemDiscoverableSchedule =
      new ItemDiscoverableSchedule(true, date, date);
    ItemBuyableSchedule changedItemBuyableSchedule = new ItemBuyableSchedule(false, date, date);
    newItemViewConfig.setItemDiscoverableSchedules(itemDiscoverableSchedule);
    newItemViewConfig.setItemBuyableSchedules(changedItemBuyableSchedule);
    newItemViewConfigSet.add(newItemViewConfig);
    itemPickupPoint.getItemViewConfig().iterator().next()
      .setItemDiscoverableSchedules(itemDiscoverableSchedule);
    itemPickupPoint.getItemViewConfig().iterator().next().setItemBuyableSchedules(null);
    boolean result = this.itemPickupPointHelperService
      .isItemViewConfigChangeForExistingChannel(itemPickupPoint, newItemViewConfigSet);
    assertFalse(result);
  }

  @Test
  public void noItemViewConfigChangeTest() {
    Date date = new Date();
    Set<ItemViewConfig> newItemViewConfigSet = new HashSet<>();
    ItemViewConfig newItemViewConfig = new ItemViewConfig();
    newItemViewConfig.setBuyable(true);
    newItemViewConfig.setDiscoverable(true);
    newItemViewConfig.setChannel(DEFAULT);
    ItemDiscoverableSchedule itemDiscoverableSchedule =
      new ItemDiscoverableSchedule(true, date, date);
    ItemBuyableSchedule changedItemBuyableSchedule = new ItemBuyableSchedule(false, date, date);
    newItemViewConfig.setItemDiscoverableSchedules(itemDiscoverableSchedule);
    newItemViewConfig.setItemBuyableSchedules(changedItemBuyableSchedule);
    newItemViewConfigSet.add(newItemViewConfig);
    itemPickupPoint.getItemViewConfig().iterator().next()
      .setItemDiscoverableSchedules(itemDiscoverableSchedule);
    itemPickupPoint.getItemViewConfig().iterator().next().setItemBuyableSchedules(null);
    boolean result = this.itemPickupPointHelperService
      .isItemViewConfigChangeForExistingChannel(itemPickupPoint, newItemViewConfigSet);
    assertFalse(result);
  }

  @Test
  public void isItemViewConfigChangeForExistingChannelTest() {
    Date date = new Date();
    Set<ItemViewConfig> newItemViewConfigSet = new HashSet<>();
    ItemViewConfig newItemViewConfig = new ItemViewConfig();
    newItemViewConfig.setBuyable(false);
    newItemViewConfig.setDiscoverable(true);
    newItemViewConfig.setChannel(CHANNEL_2);
    ItemDiscoverableSchedule itemDiscoverableSchedule =
      new ItemDiscoverableSchedule(true, date, date);
    ItemBuyableSchedule itemBuyableSchedule = new ItemBuyableSchedule(false, date, date);
    newItemViewConfig.setItemDiscoverableSchedules(itemDiscoverableSchedule);
    newItemViewConfig.setItemBuyableSchedules(itemBuyableSchedule);
    newItemViewConfigSet.add(newItemViewConfig);
    itemPickupPoint.getItemViewConfig().iterator().next()
      .setItemBuyableSchedules(itemBuyableSchedule);
    boolean result = this.itemPickupPointHelperService
      .isItemViewConfigChangeForExistingChannel(itemPickupPoint, newItemViewConfigSet);
    assertTrue(result);
  }

  @Test
  public void buyableDiscoverableScheduleNullTes() {
    Date date = new Date();
    Set<ItemViewConfig> newItemViewConfigSet = new HashSet<>();
    ItemViewConfig newItemViewConfig = new ItemViewConfig();
    newItemViewConfig.setBuyable(true);
    newItemViewConfig.setDiscoverable(true);
    newItemViewConfig.setChannel(DEFAULT);
    ItemDiscoverableSchedule itemDiscoverableSchedule =
      new ItemDiscoverableSchedule(true, date, date);
    ItemBuyableSchedule itemBuyableSchedule = new ItemBuyableSchedule(false, date, date);
    newItemViewConfig.setItemDiscoverableSchedules(itemDiscoverableSchedule);
    newItemViewConfig.setItemBuyableSchedules(itemBuyableSchedule);
    newItemViewConfigSet.add(newItemViewConfig);
    itemPickupPoint.getItemViewConfig().iterator().next()
      .setItemDiscoverableSchedules(null);
    itemPickupPoint.getItemViewConfig().iterator().next()
      .setItemBuyableSchedules(null);
    boolean result = this.itemPickupPointHelperService
      .isItemViewConfigChangeForExistingChannel(itemPickupPoint, newItemViewConfigSet);
    assertFalse(result);
  }

  @Test
  public void getAutoCreatePickupPointResponseTest() {
    ReflectionTestUtils.setField(itemPickupPointHelperService, "cncForWarehouseFeatureSwitch", true);
    Mockito.when(channelService.getDefaultChannel()).thenReturn("DEFAULT");
    Mockito.when(channelService.getCncChannel()).thenReturn("CNC");
    itemPickupPointHelperService.getAutoCreatePickupPointResponse(new AutoCreatePickupPointResponse(), itemPickupPoint);

  }

  @Test
  public void setItemViewConfigSwitchTrueTest() {
    ReflectionTestUtils.setField(itemPickupPointHelperService, "cncForWarehouseFeatureSwitch", true);
    Mockito.when(channelService.getDefaultChannel()).thenReturn("DEFAULT");
    Mockito.when(channelService.getCncChannel()).thenReturn("CNC");
    itemPickupPointHelperService.setItemViewConfig(itemPickupPoint, true, itemPickupPoint);
  }

  @Test
  public void setItemViewConfigSwitchOnDeliveryFalseTest() {
    ReflectionTestUtils.setField(itemPickupPointHelperService, "cncForWarehouseFeatureSwitch", true);
    Mockito.when(channelService.getDefaultChannel()).thenReturn("DEFAULT");
    Mockito.when(channelService.getCncChannel()).thenReturn("CNC");
    itemPickupPointHelperService.setItemViewConfig(itemPickupPoint, false, itemPickupPoint);
  }

  @Test
  public void setItemViewConfigSwitchFalseTest() {
    ReflectionTestUtils.setField(itemPickupPointHelperService, "cncForWarehouseFeatureSwitch", false);
    Mockito.when(channelService.getDefaultChannel()).thenReturn("DEFAULT");
    Mockito.when(channelService.getCncChannel()).thenReturn("CNC");
    itemPickupPointHelperService.setItemViewConfig(itemPickupPoint, true, itemPickupPoint);
  }

  @Test
  public void priceEditDisabledTrue_WithMerchantPromoTrueTest(){
    itemPickupPoint.setMerchantPromoDiscount(true);
    boolean result = itemPickupPointHelperService.isPriceEditDisabled(itemPickupPoint);
    Assertions.assertTrue(result);
  }

  @Test
  public void priceEditDisabledFalse_WithoutMerchantPromoTrueTest(){
    Price price = new Price();
    price.setListOfDiscountPrices(new ArrayList<>());
    Set<Price> prices = new HashSet<>();
    prices.add(price);
    itemPickupPoint.setMerchantPromoDiscount(false);
    itemPickupPoint.setPrice(prices);
    boolean result = itemPickupPointHelperService.isPriceEditDisabled(itemPickupPoint);
    Assertions.assertFalse(result);
  }

  @Test
  @Disabled
  public void priceEditDisabledTrue_OnGoingPromoPriceTest(){
    Price price = new Price();
    DiscountPrice discountPrice = new DiscountPrice();
    discountPrice.setDiscountPrice(10.0);
    discountPrice.setCampaignCode("CAMPAIGN");
    discountPrice.setStartDateTime(Date.from(Instant.now().minus(Duration.ofDays(1))));
    discountPrice.setEndDateTime(Date.from(Instant.now().plus(Duration.ofDays(1))));
    List<DiscountPrice> discountPriceList = new ArrayList<>();
    discountPriceList.add(discountPrice);
    Set<Price> prices = new HashSet<>();
    price.setListOfDiscountPrices(discountPriceList);
    prices.add(price);
    itemPickupPoint.setMerchantPromoDiscount(false);
    itemPickupPoint.setPrice(prices);
    boolean result = itemPickupPointHelperService.isPriceEditDisabled(itemPickupPoint);
    Assertions.assertTrue(result);
  }

  @Test
  public void priceEditDisabledFalse_EmptyCampaignCodeTest(){
    Price price = new Price();
    DiscountPrice discountPrice = new DiscountPrice();
    discountPrice.setDiscountPrice(10.0);
    discountPrice.setStartDateTime(new Date(1655280802000L));
    discountPrice.setEndDateTime(new Date(1660551202000L));
    List<DiscountPrice> discountPriceList = new ArrayList<>();
    discountPriceList.add(discountPrice);
    Set<Price> prices = new HashSet<>();
    price.setListOfDiscountPrices(discountPriceList);
    prices.add(price);
    itemPickupPoint.setMerchantPromoDiscount(false);
    itemPickupPoint.setPrice(prices);
    boolean result = itemPickupPointHelperService.isPriceEditDisabled(itemPickupPoint);
    Assertions.assertFalse(result);
  }

  @Test
  @Disabled
  public void priceEditDisabledFalse_UpcomingCampaignTest(){
    Price price = new Price();
    DiscountPrice discountPrice = new DiscountPrice();
    discountPrice.setDiscountPrice(10.0);
    discountPrice.setCampaignCode("CAMPAIGN");
    discountPrice.setStartDateTime(Date.from(Instant.now().minus(Duration.ofDays(1))));
    discountPrice.setEndDateTime(Date.from(Instant.now().plus(Duration.ofDays(1))));
    List<DiscountPrice> discountPriceList = new ArrayList<>();
    discountPriceList.add(discountPrice);
    Set<Price> prices = new HashSet<>();
    price.setListOfDiscountPrices(discountPriceList);
    prices.add(price);
    itemPickupPoint.setMerchantPromoDiscount(false);
    itemPickupPoint.setPrice(prices);
    boolean result = itemPickupPointHelperService.isPriceEditDisabled(itemPickupPoint);
    Assertions.assertTrue(result);
  }

  @Test
  public void priceEditDisabledFalse_PastCampaignTest(){
    Price price = new Price();
    DiscountPrice discountPrice = new DiscountPrice();
    discountPrice.setDiscountPrice(10.0);
    discountPrice.setCampaignCode("CAMPAIGN");
    discountPrice.setStartDateTime(new Date(1657704851000L));
    discountPrice.setEndDateTime(new Date(1657791251000L));
    List<DiscountPrice> discountPriceList = new ArrayList<>();
    discountPriceList.add(discountPrice);
    Set<Price> prices = new HashSet<>();
    price.setListOfDiscountPrices(discountPriceList);
    prices.add(price);
    itemPickupPoint.setMerchantPromoDiscount(false);
    itemPickupPoint.setPrice(prices);
    boolean result = itemPickupPointHelperService.isPriceEditDisabled(itemPickupPoint);
    Assertions.assertFalse(result);
  }

}
