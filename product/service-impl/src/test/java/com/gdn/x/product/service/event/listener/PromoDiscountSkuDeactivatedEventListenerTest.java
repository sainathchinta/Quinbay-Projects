package com.gdn.x.product.service.event.listener;

import java.util.Date;
import java.util.HashSet;
import java.util.Set;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.partners.product.pricing.streaming.model.PromoAdjustmentChangeEvent;
import com.gdn.x.product.model.entity.DiscountPrice;
import com.gdn.x.product.model.entity.Item;
import com.gdn.x.product.model.entity.Price;
import com.gdn.x.product.service.api.ItemCacheableService;
import com.gdn.x.product.service.api.ItemService;

/**
 * Created by govind on 23/04/2019 AD.
 */
public class PromoDiscountSkuDeactivatedEventListenerTest {

  private static final String STORE_ID = "10001";
  private static final String ADJUSTMENT_NAME = "adjustmentName";
  private static final String ADJUSTMENT_CODE = "adjustment-code";
  private static final String DESCRIPTION = "description";
  private static final String ITEM_SKU = "itemSku";
  private static final String MESSAGE = "message";

  private static final Date START_DATE = new Date();
  private static final Date END_DATE = new Date();
  private static final long DISCOUNT_PRICE = 7000l;
  private static final boolean IS_ACTIVATED = Boolean.TRUE;
  private PromoAdjustmentChangeEvent promoAdjustmentChangeEvent;
  private Item item;
  private Price price;
  private DiscountPrice discountPrice;
  private Set<Price> prices;

  @InjectMocks
  private PromoDiscountSkuDeactivatedEventListener promoDiscountSkuDeactivatedEventListener;

  @Mock
  private ItemCacheableService itemCacheableService;

  @Mock
  private ItemService itemService;

  @Mock
  private ObjectMapper objectMapper;

  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.openMocks(this);
    this.discountPrice = new DiscountPrice();
    this.discountPrice.setDiscountPrice(5000.0);
    this.price = new Price();
    this.price.setOfferPrice(100000.0);
    this.price.setListPrice(10000.0);
    this.price.setMerchantPromoDiscountPrice(discountPrice);
    prices = new HashSet<>();
    prices.add(price);
    this.promoAdjustmentChangeEvent =
        new PromoAdjustmentChangeEvent(STORE_ID, ADJUSTMENT_CODE, ADJUSTMENT_NAME, ITEM_SKU, ITEM_SKU, ITEM_SKU, START_DATE, END_DATE,
            DISCOUNT_PRICE, Boolean.FALSE);
    this.item = new Item();
    this.item.setItemSku(ITEM_SKU);
    this.item.setPrice(this.prices);
    Mockito.when(
        this.itemCacheableService.findItemByStoreIdAndItemSku(STORE_ID, ITEM_SKU, true, false, false, null, false))
        .thenReturn(item);
  }

  @AfterEach
  public void tearDown() throws Exception {
    Mockito.verifyNoMoreInteractions(this.itemCacheableService, this.itemService);
    Mockito.verifyNoMoreInteractions(this.objectMapper);
  }

  @Test
  public void onDomainEventConsumed_WhenNullItemSkuTest() throws Exception {
    promoAdjustmentChangeEvent.setItemSku(null);
    Mockito.when(this.objectMapper.readValue(MESSAGE, PromoAdjustmentChangeEvent.class))
      .thenReturn(promoAdjustmentChangeEvent);
    promoDiscountSkuDeactivatedEventListener.onDomainEventConsumed(MESSAGE);
    Mockito.verify(this.objectMapper).readValue(MESSAGE, PromoAdjustmentChangeEvent.class);
  }
  @Test
  public void onDomainEventConsumed_WhenNullStartDateTest() throws Exception {
    promoAdjustmentChangeEvent.setStartDate(null);
    Mockito.when(this.objectMapper.readValue(MESSAGE, PromoAdjustmentChangeEvent.class))
      .thenReturn(promoAdjustmentChangeEvent);
    promoDiscountSkuDeactivatedEventListener.onDomainEventConsumed(MESSAGE);
    Mockito.verify(this.objectMapper).readValue(MESSAGE, PromoAdjustmentChangeEvent.class);
  }

  @Test
  public void onDomainEventConsumed_WhenNullEndDateTest() throws Exception {
    promoAdjustmentChangeEvent.setEndDate(null);
    Mockito.when(this.objectMapper.readValue(MESSAGE, PromoAdjustmentChangeEvent.class))
      .thenReturn(promoAdjustmentChangeEvent);
    promoDiscountSkuDeactivatedEventListener.onDomainEventConsumed(MESSAGE);
    Mockito.verify(this.objectMapper).readValue(MESSAGE, PromoAdjustmentChangeEvent.class);
  }

  @Test public void onDomainEventConsumed_WhenNullItemTest() throws Exception {
    Mockito.when(
        this.itemCacheableService.findItemByStoreIdAndItemSku(STORE_ID, ITEM_SKU, true, false, false, null, false))
        .thenReturn(null);
    Mockito.when(this.objectMapper.readValue(MESSAGE, PromoAdjustmentChangeEvent.class))
      .thenReturn(promoAdjustmentChangeEvent);
    promoDiscountSkuDeactivatedEventListener.onDomainEventConsumed(MESSAGE);
    Mockito.verify(this.objectMapper).readValue(MESSAGE, PromoAdjustmentChangeEvent.class);
    Mockito.verify(this.itemCacheableService)
        .findItemByStoreIdAndItemSku(STORE_ID, ITEM_SKU, true, false, false, null, false);
  }

  @Test
  public void onDomainEventConsumedInActivePromotionTest() throws Exception {
    Mockito.when(this.objectMapper.readValue(MESSAGE, PromoAdjustmentChangeEvent.class))
      .thenReturn(promoAdjustmentChangeEvent);
    promoDiscountSkuDeactivatedEventListener.onDomainEventConsumed(MESSAGE);
    Mockito.verify(this.objectMapper).readValue(MESSAGE, PromoAdjustmentChangeEvent.class);
    Mockito.verify(this.itemCacheableService)
        .findItemByStoreIdAndItemSku(STORE_ID, ITEM_SKU, true, false, false, null, false);
    Mockito.verify(itemService)
        .updateItemMerchantDiscountPrice(Mockito.eq(STORE_ID), Mockito.eq(item));
    Assertions.assertNull(item.getPrice().iterator().next().getMerchantPromoDiscountPrice());
  }

  @Test
  public void onDomainEventConsumed_WhenPromoStatusIsActiveTest() throws Exception {
    promoAdjustmentChangeEvent.setActivated(true);
    Mockito.when(this.objectMapper.readValue(MESSAGE, PromoAdjustmentChangeEvent.class))
      .thenReturn(promoAdjustmentChangeEvent);
    promoDiscountSkuDeactivatedEventListener.onDomainEventConsumed(MESSAGE);
    Mockito.verify(this.objectMapper).readValue(MESSAGE, PromoAdjustmentChangeEvent.class);
  }

  @Test
  public void onDomainEventConsumed_WhenStoreIdNullTest() throws Exception {
    promoAdjustmentChangeEvent.setStoreId(null);
    Mockito.when(this.objectMapper.readValue(MESSAGE, PromoAdjustmentChangeEvent.class))
      .thenReturn(promoAdjustmentChangeEvent);
    promoDiscountSkuDeactivatedEventListener.onDomainEventConsumed(MESSAGE);
    Mockito.verify(this.objectMapper).readValue(MESSAGE, PromoAdjustmentChangeEvent.class);
  }

}
