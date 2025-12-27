package com.gdn.x.product.service.event.listener;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import com.gdn.x.product.enums.RetryPublishStatus;
import com.gdn.x.product.model.entity.ProductRetryEventPublish;
import com.gdn.x.product.service.api.ProductRetryEventPublishService;
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
import org.mockito.MockitoAnnotations;
import org.springframework.test.util.ReflectionTestUtils;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.x.product.enums.SystemParameterNames;
import com.gdn.x.product.model.entity.DiscountPrice;
import com.gdn.x.product.model.entity.Item;
import com.gdn.x.product.model.entity.ItemPickupPoint;
import com.gdn.x.product.model.entity.Price;
import com.gdn.x.product.model.entity.SystemParameter;
import com.gdn.x.product.service.api.ItemPickupPointService;
import com.gdn.x.product.service.api.ProductAndItemSolrIndexerService;
import com.gdn.x.product.service.api.SystemParameterService;
import com.gdn.x.promotion.domain.event.model.AdjustmentProductChange;

public class AdjustmentProductChangeListenerV2Test {

  private static final String PICKUP_POINT_CODE = "pickupPointCode";
  @InjectMocks
  private AdjustmentProductChangeListenerV2 adjustmentProductChangeListener;

  @Mock
  private ItemPickupPointService itemPickupPointService;

  @Mock
  private SystemParameterService systemParameterService;

  @Mock
  private ProductAndItemSolrIndexerService productAndItemSolrIndexerService;

  @Mock
  private ObjectMapper objectMapper;

  @Mock
  private ProductRetryEventPublishService retryEventPublishService;

  @Captor
  private ArgumentCaptor<ItemPickupPoint> itemPickupPointArgumentCaptor;

  private static final String STORE_ID = "10001";
  private static final String BP_CODE = "BP-code";
  private static final String PP_CODE = "BP-code";
  private static final String ADJUSTMENT_NAME = "adjustmentName";
  private static final String MERCHANT_CODE = "merchantCode";
  private static final String DESCRIPTION = "description";
  private static final String ITEM_SKU = "itemSku";
  private static final String CAMPAIGN_CODE = "CAMPAIGN_CODE";
  private static final Date START_DATE = new Date();
  private static final Date END_DATE = new Date();
  private static final long DISCOUNT_PRICE = 7000l;
  private static final boolean IS_ACTIVATED = Boolean.TRUE;
  private static final String MESSAGE = "message";

  private static final String TOPIC_NAME = "com.gdn.x.promotion.adjustment.product.change.v2";

  private AdjustmentProductChange adjustmentProductChange;
  private Item item;
  private ItemPickupPoint itemPickupPoint;
  private Price price;
  private DiscountPrice discountPrice;
  private List<DiscountPrice> discountPrices;
  private Set<Price> prices;
  private ProductRetryEventPublish eventForRetry;


  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.openMocks(this);
    ReflectionTestUtils.setField(adjustmentProductChangeListener, "v2Enabled", true);
    itemPickupPoint = new ItemPickupPoint();
    itemPickupPoint.setItemSku(ITEM_SKU);
    itemPickupPoint.setPickupPointCode(ITEM_SKU);
    eventForRetry =
      ProductRetryEventPublish.builder().retryCount(0).identifier(ITEM_SKU).secondaryIdentifier(PICKUP_POINT_CODE)
      .retryPublishStatus(RetryPublishStatus.PENDING).topicName(TOPIC_NAME)
      .clearCache(Boolean.FALSE).build();
    this.discountPrice = new DiscountPrice();
    this.discountPrice.setDiscountPrice(5000.0);
    discountPrices = new ArrayList<>();
    discountPrices.add(discountPrice);
    this.price = new Price();
    this.price.setOfferPrice(100000.0);
    this.price.setListPrice(10000.0);
    this.price.setListOfDiscountPrices(discountPrices);
    prices = new HashSet<>();
    prices.add(price);
    itemPickupPoint.setPrice(prices);
    this.adjustmentProductChange =
        new AdjustmentProductChange(ADJUSTMENT_NAME, DESCRIPTION, ITEM_SKU, CAMPAIGN_CODE, ITEM_SKU, START_DATE,
            END_DATE, DISCOUNT_PRICE, IS_ACTIVATED, false, 0, 1, PICKUP_POINT_CODE);
    this.item = new Item();
    this.item.setItemSku(ITEM_SKU);
    this.item.setPrice(this.prices);
    SystemParameter systemParameter = new SystemParameter(STORE_ID,
        SystemParameterNames.LISTEN_TO_ADJUSTMENT_PRODUCT_CHANGE_EVENT, "true", StringUtils.EMPTY);
    Mockito.when(systemParameterService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.LISTEN_TO_ADJUSTMENT_PRODUCT_CHANGE_EVENT)).thenReturn(systemParameter);
    Mockito.when(itemPickupPointService.findByItemSkuAndDelivery(Mockito.anyString(), Mockito.anyString()))
        .thenReturn(itemPickupPoint);
    Mockito.when(retryEventPublishService.insertToRetryPublish(eventForRetry)).thenReturn(eventForRetry);
  }

  @AfterEach
  public void tearDown() throws Exception {
    Mockito.verifyNoMoreInteractions(systemParameterService);
    Mockito.verifyNoMoreInteractions(productAndItemSolrIndexerService);
    Mockito.verifyNoMoreInteractions(itemPickupPointService);
    Mockito.verifyNoMoreInteractions(objectMapper);
    Mockito.verifyNoMoreInteractions(retryEventPublishService);
  }

  @Test
  public void testOnDomainEventConsumedV2NotEnabled() {
    ReflectionTestUtils.setField(adjustmentProductChangeListener, "v2Enabled", false);
    adjustmentProductChangeListener.onDomainEventConsumed(MESSAGE);
  }

  @Test
  public void testOnDomainEventConsumedV2NotEnabledSysParamTest() {
    ReflectionTestUtils.setField(adjustmentProductChangeListener, "v2Enabled", true);
    SystemParameter systemParameter = new SystemParameter(STORE_ID,
        SystemParameterNames.LISTEN_TO_ADJUSTMENT_PRODUCT_CHANGE_EVENT, "false", StringUtils.EMPTY);
    Mockito.when(systemParameterService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.LISTEN_TO_ADJUSTMENT_PRODUCT_CHANGE_EVENT)).thenReturn(systemParameter);
    adjustmentProductChangeListener.onDomainEventConsumed(MESSAGE);
    Mockito.verify(systemParameterService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.LISTEN_TO_ADJUSTMENT_PRODUCT_CHANGE_EVENT);
  }

  @Test
  public void testOnDomainEventConsumedV2EnabledNullItemSkuTest() throws IOException {
    ReflectionTestUtils.setField(adjustmentProductChangeListener, "v2Enabled", true);
    adjustmentProductChange.setProductSku(null);
    eventForRetry.setIdentifier(null);
    Mockito.when(this.objectMapper.readValue(MESSAGE, AdjustmentProductChange.class))
        .thenReturn(adjustmentProductChange);
    adjustmentProductChangeListener.onDomainEventConsumed(MESSAGE);
    Mockito.verify(systemParameterService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.LISTEN_TO_ADJUSTMENT_PRODUCT_CHANGE_EVENT);
    Mockito.verify(objectMapper).readValue(MESSAGE, AdjustmentProductChange.class);
    Mockito.verify(retryEventPublishService).insertToRetryPublish(eventForRetry);
  }

  @Test
  public void testOnDomainEventConsumedV2EnabledNullStartDateTest() throws IOException {
    ReflectionTestUtils.setField(adjustmentProductChangeListener, "v2Enabled", true);
    adjustmentProductChange.setStartDate(null);
    Mockito.when(this.objectMapper.readValue(MESSAGE, AdjustmentProductChange.class))
        .thenReturn(adjustmentProductChange);
    adjustmentProductChangeListener.onDomainEventConsumed(MESSAGE);
    Mockito.verify(systemParameterService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.LISTEN_TO_ADJUSTMENT_PRODUCT_CHANGE_EVENT);
    Mockito.verify(objectMapper).readValue(MESSAGE, AdjustmentProductChange.class);
    Mockito.verify(retryEventPublishService).insertToRetryPublish(eventForRetry);
  }

  @Test
  public void testOnDomainEventConsumedV2EnabledNullEndDateTest() throws IOException {
    ReflectionTestUtils.setField(adjustmentProductChangeListener, "v2Enabled", true);
    adjustmentProductChange.setEndDate(null);
    Mockito.when(this.objectMapper.readValue(MESSAGE, AdjustmentProductChange.class))
        .thenReturn(adjustmentProductChange);
    adjustmentProductChangeListener.onDomainEventConsumed(MESSAGE);
    Mockito.verify(systemParameterService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.LISTEN_TO_ADJUSTMENT_PRODUCT_CHANGE_EVENT);
    Mockito.verify(objectMapper).readValue(MESSAGE, AdjustmentProductChange.class);
    Mockito.verify(retryEventPublishService).insertToRetryPublish(eventForRetry);
  }

  @Test
  public void testOnDomainEventConsumedV2EnabledL4Test() throws IOException {
    ReflectionTestUtils.setField(adjustmentProductChangeListener, "v2Enabled", true);
    adjustmentProductChange.setPickupPointCode(null);
    Mockito.when(this.objectMapper.readValue(MESSAGE, AdjustmentProductChange.class))
        .thenReturn(adjustmentProductChange);
    Mockito.when(itemPickupPointService.findByItemSkuAndDelivery(Mockito.anyString(), Mockito.anyString()))
        .thenReturn(itemPickupPoint);
    adjustmentProductChangeListener.onDomainEventConsumed(MESSAGE);
    Mockito.verify(systemParameterService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.LISTEN_TO_ADJUSTMENT_PRODUCT_CHANGE_EVENT);
    Mockito.verify(objectMapper).readValue(MESSAGE, AdjustmentProductChange.class);
    Mockito.verify(itemPickupPointService).findByItemSkuAndDelivery(Mockito.anyString(), Mockito.anyString());
    Mockito.verify(itemPickupPointService).saveItemPickupPoint(Mockito.any(ItemPickupPoint.class));
    Mockito.verify(productAndItemSolrIndexerService)
        .updateSolrOnPromoFlagChangeByItemSkus(Mockito.anyMap(), Mockito.anyBoolean(),
          Mockito.isNull(), Mockito.anyMap());
  }

  @Test
  public void testOnDomainEventConsumedV2EnabledL4PromoAdjustmentBlackListTest() throws IOException {
    ReflectionTestUtils.setField(adjustmentProductChangeListener, "v2Enabled", true);
    ReflectionTestUtils.setField(adjustmentProductChangeListener,"blacklistPromoAdjustmentReindexEnabled",true);
    ReflectionTestUtils.setField(adjustmentProductChangeListener,"blackListSellersForL3Reindex","itemSku");
    adjustmentProductChange.setPickupPointCode(null);
    Mockito.when(this.objectMapper.readValue(MESSAGE, AdjustmentProductChange.class))
        .thenReturn(adjustmentProductChange);
    Mockito.when(itemPickupPointService.findByItemSkuAndDelivery(Mockito.anyString(), Mockito.anyString()))
        .thenReturn(itemPickupPoint);
    adjustmentProductChangeListener.onDomainEventConsumed(MESSAGE);
    Mockito.verify(systemParameterService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.LISTEN_TO_ADJUSTMENT_PRODUCT_CHANGE_EVENT);
    Mockito.verify(objectMapper).readValue(MESSAGE, AdjustmentProductChange.class);
    Mockito.verify(itemPickupPointService).findByItemSkuAndDelivery(Mockito.anyString(), Mockito.anyString());
    Mockito.verify(itemPickupPointService).saveItemPickupPoint(Mockito.any(ItemPickupPoint.class));
  }

  @Test
  public void testOnDomainEventConsumedV2EnabledL4PromoAdjustmentBlackListMatchingTest() throws IOException {
    ReflectionTestUtils.setField(adjustmentProductChangeListener, "v2Enabled", true);
    ReflectionTestUtils.setField(adjustmentProductChangeListener,"blacklistPromoAdjustmentReindexEnabled",true);
    ReflectionTestUtils.setField(adjustmentProductChangeListener,"blackListSellersForL3Reindex","something");
    adjustmentProductChange.setPickupPointCode(null);
    Mockito.when(this.objectMapper.readValue(MESSAGE, AdjustmentProductChange.class))
        .thenReturn(adjustmentProductChange);
    Mockito.when(itemPickupPointService.findByItemSkuAndDelivery(Mockito.anyString(), Mockito.anyString()))
        .thenReturn(itemPickupPoint);
    adjustmentProductChangeListener.onDomainEventConsumed(MESSAGE);
    Mockito.verify(systemParameterService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.LISTEN_TO_ADJUSTMENT_PRODUCT_CHANGE_EVENT);
    Mockito.verify(objectMapper).readValue(MESSAGE, AdjustmentProductChange.class);
    Mockito.verify(itemPickupPointService).findByItemSkuAndDelivery(Mockito.anyString(), Mockito.anyString());
    Mockito.verify(itemPickupPointService).saveItemPickupPoint(Mockito.any(ItemPickupPoint.class));
    Mockito.verify(productAndItemSolrIndexerService)
        .updateSolrOnPromoFlagChangeByItemSkus(Mockito.anyMap(), Mockito.anyBoolean(),
            Mockito.isNull(), Mockito.anyMap());
  }

  @Test
  public void testOnDomainEventConsumedV2EnabledL4PromoAdjustmentBlackListMatchingTest2() throws IOException {
    ReflectionTestUtils.setField(adjustmentProductChangeListener, "v2Enabled", true);
    ReflectionTestUtils.setField(adjustmentProductChangeListener,"blacklistPromoAdjustmentReindexEnabled",false);
    adjustmentProductChange.setPickupPointCode(null);
    Mockito.when(this.objectMapper.readValue(MESSAGE, AdjustmentProductChange.class))
        .thenReturn(adjustmentProductChange);
    Mockito.when(itemPickupPointService.findByItemSkuAndDelivery(Mockito.anyString(), Mockito.anyString()))
        .thenReturn(itemPickupPoint);
    adjustmentProductChangeListener.onDomainEventConsumed(MESSAGE);
    Mockito.verify(systemParameterService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.LISTEN_TO_ADJUSTMENT_PRODUCT_CHANGE_EVENT);
    Mockito.verify(objectMapper).readValue(MESSAGE, AdjustmentProductChange.class);
    Mockito.verify(itemPickupPointService).findByItemSkuAndDelivery(Mockito.anyString(), Mockito.anyString());
    Mockito.verify(itemPickupPointService).saveItemPickupPoint(Mockito.any(ItemPickupPoint.class));
    Mockito.verify(productAndItemSolrIndexerService)
        .updateSolrOnPromoFlagChangeByItemSkus(Mockito.anyMap(), Mockito.anyBoolean(),
            Mockito.isNull(), Mockito.anyMap());
  }

  @Test
  public void testOnDomainEventConsumedV2EnabledL5Test() throws IOException {
    ReflectionTestUtils.setField(adjustmentProductChangeListener, "v2Enabled", true);
    adjustmentProductChange.setPickupPointCode(PP_CODE);
    Mockito.when(this.objectMapper.readValue(MESSAGE, AdjustmentProductChange.class))
        .thenReturn(adjustmentProductChange);
    Mockito.when(itemPickupPointService.findByItemSkuAndPickupPointCodeFromDb(Mockito.anyString(), Mockito.anyString(),
        Mockito.anyString())).thenReturn(itemPickupPoint);
    adjustmentProductChangeListener.onDomainEventConsumed(MESSAGE);
    Mockito.verify(systemParameterService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.LISTEN_TO_ADJUSTMENT_PRODUCT_CHANGE_EVENT);
    Mockito.verify(objectMapper).readValue(MESSAGE, AdjustmentProductChange.class);
    Mockito.verify(itemPickupPointService)
        .findByItemSkuAndPickupPointCodeFromDb(Mockito.anyString(), Mockito.anyString(), Mockito.anyString());
    Mockito.verify(itemPickupPointService).saveItemPickupPoint(Mockito.any(ItemPickupPoint.class));
    Mockito.verify(productAndItemSolrIndexerService)
        .updateSolrOnPromoFlagChangeByItemSkus(Mockito.anyMap(), Mockito.anyBoolean(), Mockito.isNull(), Mockito.anyMap());
  }

  @Test
  public void testOnDomainEventConsumedV2EnabledL5ActivatedTest() throws IOException {
    ReflectionTestUtils.setField(adjustmentProductChangeListener, "v2Enabled", true);
    adjustmentProductChange.setPickupPointCode(PP_CODE);
    adjustmentProductChange.setActivated(true);
    itemPickupPoint.getPrice().stream().findFirst().get().setListOfDiscountPrices(new ArrayList<>());
    Mockito.when(this.objectMapper.readValue(MESSAGE, AdjustmentProductChange.class))
        .thenReturn(adjustmentProductChange);
    Mockito.when(itemPickupPointService.findByItemSkuAndPickupPointCodeFromDb(Mockito.anyString(), Mockito.anyString(),
        Mockito.anyString())).thenReturn(itemPickupPoint);
    adjustmentProductChangeListener.onDomainEventConsumed(MESSAGE);
    Mockito.verify(systemParameterService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.LISTEN_TO_ADJUSTMENT_PRODUCT_CHANGE_EVENT);
    Mockito.verify(objectMapper).readValue(MESSAGE, AdjustmentProductChange.class);
    Mockito.verify(itemPickupPointService)
        .findByItemSkuAndPickupPointCodeFromDb(Mockito.anyString(), Mockito.anyString(), Mockito.anyString());
    Mockito.verify(itemPickupPointService).saveItemPickupPoint(Mockito.any(ItemPickupPoint.class));
    Mockito.verify(productAndItemSolrIndexerService)
        .updateSolrOnPromoFlagChangeByItemSkus(Mockito.anyMap(), Mockito.anyBoolean(),
          Mockito.isNull(), Mockito.anyMap());
  }

  @Test
  public void testOnDomainEventConsumedV2EnabledL5DeActivatedTest() throws IOException {
    ReflectionTestUtils.setField(adjustmentProductChangeListener, "v2Enabled", true);
    adjustmentProductChange.setPickupPointCode(PP_CODE);
    adjustmentProductChange.setActivated(false);
    itemPickupPoint.getPrice().stream().findFirst().get().setListOfDiscountPrices(Arrays.asList(discountPrice));
    Mockito.when(this.objectMapper.readValue(MESSAGE, AdjustmentProductChange.class))
        .thenReturn(adjustmentProductChange);
    Mockito.when(itemPickupPointService.findByItemSkuAndPickupPointCodeFromDb(Mockito.anyString(), Mockito.anyString(),
        Mockito.anyString())).thenReturn(itemPickupPoint);
    adjustmentProductChangeListener.onDomainEventConsumed(MESSAGE);
    Mockito.verify(systemParameterService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.LISTEN_TO_ADJUSTMENT_PRODUCT_CHANGE_EVENT);
    Mockito.verify(objectMapper).readValue(MESSAGE, AdjustmentProductChange.class);
    Mockito.verify(itemPickupPointService)
        .findByItemSkuAndPickupPointCodeFromDb(Mockito.anyString(), Mockito.anyString(), Mockito.anyString());
    Mockito.verify(itemPickupPointService).saveItemPickupPoint(Mockito.any(ItemPickupPoint.class));
    Mockito.verify(productAndItemSolrIndexerService)
        .updateSolrOnPromoFlagChangeByItemSkus(Mockito.anyMap(), Mockito.anyBoolean(), Mockito.isNull(), Mockito.anyMap());
  }

  @Test
  public void testOnDomainEventConsumedV2EnabledL5ActivatedWithDiscountTest() throws IOException {
    ReflectionTestUtils.setField(adjustmentProductChangeListener, "v2Enabled", true);
    adjustmentProductChange.setPickupPointCode(PP_CODE);
    adjustmentProductChange.setActivated(true);
    adjustmentProductChange.setCampaignCode(discountPrice.getCampaignCode());
    adjustmentProductChange.setExclusiveProduct(true);
    itemPickupPoint.getPrice().stream().findFirst().get()
        .setListOfDiscountPrices(new ArrayList<>(Arrays.asList(discountPrice)));
    Mockito.when(this.objectMapper.readValue(MESSAGE, AdjustmentProductChange.class))
        .thenReturn(adjustmentProductChange);
    Mockito.when(itemPickupPointService.findByItemSkuAndPickupPointCodeFromDb(Mockito.anyString(), Mockito.anyString(),
        Mockito.anyString())).thenReturn(itemPickupPoint);
    adjustmentProductChangeListener.onDomainEventConsumed(MESSAGE);
    Mockito.verify(systemParameterService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.LISTEN_TO_ADJUSTMENT_PRODUCT_CHANGE_EVENT);
    Mockito.verify(objectMapper).readValue(MESSAGE, AdjustmentProductChange.class);
    Mockito.verify(itemPickupPointService)
        .findByItemSkuAndPickupPointCodeFromDb(Mockito.anyString(), Mockito.anyString(), Mockito.anyString());
    Mockito.verify(itemPickupPointService).saveItemPickupPoint(itemPickupPointArgumentCaptor.capture());
    Mockito.verify(productAndItemSolrIndexerService)
        .updateSolrOnPromoFlagChangeByItemSkus(Mockito.anyMap(), Mockito.anyBoolean(), Mockito.isNull(), Mockito.anyMap());
    Assertions.assertEquals(1,
        itemPickupPointArgumentCaptor.getValue().getPrice().stream().findFirst().get().getListOfDiscountPrices()
            .size());
    Assertions.assertTrue(
        itemPickupPointArgumentCaptor.getValue().getPrice().stream().findFirst().get().getListOfDiscountPrices()
            .get(0).isExclusiveProduct());
  }

  @Test
  public void testOnDomainEventConsumedV2EnabledL5ActivatedWithAddDiscountTest() throws IOException {
    ReflectionTestUtils.setField(adjustmentProductChangeListener, "v2Enabled", true);
    adjustmentProductChange.setPickupPointCode(PP_CODE);
    adjustmentProductChange.setActivated(true);
    adjustmentProductChange.setCampaignCode(PP_CODE);
    itemPickupPoint.getPrice().stream().findFirst().get()
        .setListOfDiscountPrices(new ArrayList<>(Arrays.asList(discountPrice)));
    Mockito.when(this.objectMapper.readValue(MESSAGE, AdjustmentProductChange.class))
        .thenReturn(adjustmentProductChange);
    Mockito.when(itemPickupPointService.findByItemSkuAndPickupPointCodeFromDb(Mockito.anyString(), Mockito.anyString(),
        Mockito.anyString())).thenReturn(itemPickupPoint);
    adjustmentProductChangeListener.onDomainEventConsumed(MESSAGE);
    Mockito.verify(systemParameterService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.LISTEN_TO_ADJUSTMENT_PRODUCT_CHANGE_EVENT);
    Mockito.verify(objectMapper).readValue(MESSAGE, AdjustmentProductChange.class);
    Mockito.verify(itemPickupPointService)
        .findByItemSkuAndPickupPointCodeFromDb(Mockito.anyString(), Mockito.anyString(), Mockito.anyString());
    Mockito.verify(itemPickupPointService).saveItemPickupPoint(itemPickupPointArgumentCaptor.capture());
    Mockito.verify(productAndItemSolrIndexerService)
        .updateSolrOnPromoFlagChangeByItemSkus(Mockito.anyMap(), Mockito.anyBoolean(), Mockito.isNull(), Mockito.anyMap());
    Assertions.assertEquals(2,
        itemPickupPointArgumentCaptor.getValue().getPrice().stream().findFirst().get().getListOfDiscountPrices()
            .size());
  }

  @Test
  public void testOnDomainEventConsumedV2EnabledL5ActivatedWithMFD_trueTest() throws IOException {
    itemPickupPoint.setMarkForDelete(true);
    ReflectionTestUtils.setField(adjustmentProductChangeListener, "v2Enabled", true);
    adjustmentProductChange.setPickupPointCode(PP_CODE);
    adjustmentProductChange.setActivated(true);
    itemPickupPoint.getPrice().stream().findFirst().get().setListOfDiscountPrices(new ArrayList<>());
    Mockito.when(this.objectMapper.readValue(MESSAGE, AdjustmentProductChange.class))
      .thenReturn(adjustmentProductChange);
    Mockito.when(itemPickupPointService.findByItemSkuAndPickupPointCodeFromDb(Mockito.anyString(), Mockito.anyString(),
      Mockito.anyString())).thenReturn(itemPickupPoint);
    adjustmentProductChangeListener.onDomainEventConsumed(MESSAGE);
    Mockito.verify(systemParameterService)
      .findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.LISTEN_TO_ADJUSTMENT_PRODUCT_CHANGE_EVENT);
    Mockito.verify(objectMapper).readValue(MESSAGE, AdjustmentProductChange.class);
    Mockito.verify(itemPickupPointService)
      .findByItemSkuAndPickupPointCodeFromDb(Mockito.anyString(), Mockito.anyString(), Mockito.anyString());
  }
}
