package com.gdn.x.product.service.event.listener;

import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.MockitoAnnotations.openMocks;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.springframework.test.util.ReflectionTestUtils;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.partners.product.pricing.streaming.model.promo.bundling.PromoBundlingStatusChangedEventModel;
import com.gdn.x.product.service.api.ItemService;

public class PromoBundlingStatusChangedEventListenerTest {

  @InjectMocks
  private PromoBundlingStatusChangedEventListener promoBundlingStatusChangedEventListener;

  @Mock
  private ItemService itemService;

  @Mock
  private ObjectMapper objectMapper;

  private PromoBundlingStatusChangedEventModel promoBundlingStatusChangedEventModel;

  private static final String STORE_ID = "10001";
  private static final String MAIN_ITEM_SKU = "mainItemSku";
  private static final String PROMO_BUNDLING_TYPE = "promoBundlingType";
  private static final String MESSAGE = "message";

  @BeforeEach
  public void setUp(){
    openMocks(this);
    promoBundlingStatusChangedEventModel = new PromoBundlingStatusChangedEventModel();
    promoBundlingStatusChangedEventModel.setStoreId(STORE_ID);
    promoBundlingStatusChangedEventModel.setPromoBundlingType(PROMO_BUNDLING_TYPE);
    promoBundlingStatusChangedEventModel.setMainItemSku(MAIN_ITEM_SKU);
    ReflectionTestUtils.setField(promoBundlingStatusChangedEventListener,
      "promoEventPricingEnabled", true);
  }

  @AfterEach
  public void tearDown() throws Exception {
    verifyNoMoreInteractions(this.itemService);
    verifyNoMoreInteractions(this.objectMapper);
  }


  @Test
  public void onDomainEventConsumed() throws Exception {
    Mockito.when(this.objectMapper.readValue(MESSAGE, PromoBundlingStatusChangedEventModel.class))
      .thenReturn(promoBundlingStatusChangedEventModel);
    promoBundlingStatusChangedEventListener.onDomainEventConsumed(MESSAGE);
    Mockito.verify(itemService)
        .processPromoBundlingStatusChangedEventInItemPickupPoint(STORE_ID, MAIN_ITEM_SKU, PROMO_BUNDLING_TYPE, false, null);
    Mockito.verify(this.objectMapper).readValue(MESSAGE,
      PromoBundlingStatusChangedEventModel.class);
  }

  @Test
  public void onDomainEventConsumedStoreIdNullTest() throws Exception {
    promoBundlingStatusChangedEventModel.setStoreId(null);
    Mockito.when(this.objectMapper.readValue(MESSAGE, PromoBundlingStatusChangedEventModel.class))
      .thenReturn(promoBundlingStatusChangedEventModel);
    promoBundlingStatusChangedEventListener.onDomainEventConsumed(MESSAGE);
    Mockito.verify(this.objectMapper).readValue(MESSAGE,
      PromoBundlingStatusChangedEventModel.class);
  }

  @Test
  public void onDomainEventConsumedItemSkuNull() throws Exception {
    promoBundlingStatusChangedEventModel.setMainItemSku(null);
    Mockito.when(this.objectMapper.readValue(MESSAGE, PromoBundlingStatusChangedEventModel.class))
      .thenReturn(promoBundlingStatusChangedEventModel);
    promoBundlingStatusChangedEventListener.onDomainEventConsumed(MESSAGE);
    Mockito.verify(this.objectMapper).readValue(MESSAGE,
      PromoBundlingStatusChangedEventModel.class);
  }

  @Test
  public void onDomainEventConsumedPromoBundlingTypeNull() throws Exception {
    promoBundlingStatusChangedEventModel.setPromoBundlingType(null);
    Mockito.when(this.objectMapper.readValue(MESSAGE, PromoBundlingStatusChangedEventModel.class))
      .thenReturn(promoBundlingStatusChangedEventModel);
    promoBundlingStatusChangedEventListener.onDomainEventConsumed(MESSAGE);
    Mockito.verify(this.objectMapper).readValue(MESSAGE,
      PromoBundlingStatusChangedEventModel.class);
  }

  @Test
  public void onDomainEventConsumedIgnoreTrue() throws Exception {
    promoBundlingStatusChangedEventModel.setIgnore(true);
    Mockito.when(this.objectMapper.readValue(MESSAGE, PromoBundlingStatusChangedEventModel.class))
      .thenReturn(promoBundlingStatusChangedEventModel);
    promoBundlingStatusChangedEventListener.onDomainEventConsumed(MESSAGE);
    Mockito.verify(this.objectMapper).readValue(MESSAGE,
      PromoBundlingStatusChangedEventModel.class);
  }

  @Test
  public void onDomainEventConsumedPromoEventPricingEnabledFalse() throws Exception {
    ReflectionTestUtils.setField(promoBundlingStatusChangedEventListener, "promoEventPricingEnabled", false);
    Mockito.when(this.objectMapper.readValue(MESSAGE, PromoBundlingStatusChangedEventModel.class))
      .thenReturn(promoBundlingStatusChangedEventModel);
    promoBundlingStatusChangedEventListener.onDomainEventConsumed(MESSAGE);
    Mockito.verify(this.objectMapper).readValue(MESSAGE,
      PromoBundlingStatusChangedEventModel.class);
  }

  @Test
  public void onDomainEventConsumedExceptionTest() throws Exception {
    Mockito.doThrow(RuntimeException.class).when(itemService)
        .processPromoBundlingStatusChangedEventInItemPickupPoint(STORE_ID, MAIN_ITEM_SKU, PROMO_BUNDLING_TYPE, false, null);
    Mockito.when(this.objectMapper.readValue(MESSAGE, PromoBundlingStatusChangedEventModel.class))
      .thenReturn(promoBundlingStatusChangedEventModel);
    promoBundlingStatusChangedEventListener.onDomainEventConsumed(MESSAGE);
    Mockito.verify(itemService)
        .processPromoBundlingStatusChangedEventInItemPickupPoint(STORE_ID, MAIN_ITEM_SKU, PROMO_BUNDLING_TYPE, false, null);
    Mockito.verify(this.objectMapper).readValue(MESSAGE,
      PromoBundlingStatusChangedEventModel.class);
  }

  @Test
  public void onDomainEventConsumedWHActivatedAndPromoBundlingTrue() throws Exception {
    promoBundlingStatusChangedEventModel.setWholesalePriceActivated(true);
    promoBundlingStatusChangedEventModel.setPromoBundlingActivated(true);
    Mockito.when(this.objectMapper.readValue(MESSAGE, PromoBundlingStatusChangedEventModel.class))
      .thenReturn(promoBundlingStatusChangedEventModel);
    promoBundlingStatusChangedEventListener.onDomainEventConsumed(MESSAGE);
    Mockito.verify(this.objectMapper).readValue(MESSAGE,
      PromoBundlingStatusChangedEventModel.class);
  }
}