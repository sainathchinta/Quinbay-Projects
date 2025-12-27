package com.gdn.x.product.service.event.listener;

import org.apache.commons.lang3.StringUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.partners.product.pricing.streaming.model.promo.bundling.PromoBundlingActivatedEvent;
import com.gdn.x.product.service.api.ItemService;

public class PromoBundlingActivatedEventListenerTest {

  private final String ITEM_SKU = "item-sku";

  private final String PROMO_BUNDLING_TYPE = "promo-bundling-type";

  private final String STORE_ID = "store-id";

  private static final String MESSAGE = "payload";

  private PromoBundlingActivatedEvent promoBundlingActivatedEvent =
    new PromoBundlingActivatedEvent();

  @InjectMocks
  private PromoBundlingActivatedEventListener listener;

  @Mock
  private ItemService itemService;

  @Mock
  private ObjectMapper objectMapper;

  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.openMocks(this);
    promoBundlingActivatedEvent.setStoreId(STORE_ID);
    promoBundlingActivatedEvent.setMainItemSku(ITEM_SKU);
    promoBundlingActivatedEvent.setPromoBundlingType(PROMO_BUNDLING_TYPE);
  }

  @AfterEach
  public void tearDown() throws Exception {
    Mockito.verifyNoMoreInteractions(this.itemService);
    Mockito.verifyNoMoreInteractions(this.objectMapper);
  }

  @Test
  public void onDomainEventConsumedTest() throws Exception {
    Mockito.when(this.objectMapper.readValue(MESSAGE, PromoBundlingActivatedEvent.class))
      .thenReturn(promoBundlingActivatedEvent);
    listener.onDomainEventConsumed(MESSAGE);
    Mockito.verify(this.itemService)
      .addActivePromoBundling(STORE_ID, ITEM_SKU, PROMO_BUNDLING_TYPE);
    Mockito.verify(this.objectMapper).readValue(MESSAGE, PromoBundlingActivatedEvent.class);
  }

  @Test
  public void onDomainEventConsumed_exceptionTest() throws Exception {
    Mockito.when(this.objectMapper.readValue(MESSAGE, PromoBundlingActivatedEvent.class))
      .thenReturn(promoBundlingActivatedEvent);
    Mockito.doThrow(RuntimeException.class).when(this.itemService)
      .addActivePromoBundling(STORE_ID, ITEM_SKU, PROMO_BUNDLING_TYPE);
    listener.onDomainEventConsumed(MESSAGE);
    Mockito.verify(this.itemService)
      .addActivePromoBundling(STORE_ID, ITEM_SKU, PROMO_BUNDLING_TYPE);
    Mockito.verify(this.objectMapper).readValue(MESSAGE, PromoBundlingActivatedEvent.class);
  }

  @Test
  public void onDomainEventConsumed_storeIdEmptyTest() throws Exception {
    promoBundlingActivatedEvent.setStoreId(StringUtils.EMPTY);
    Mockito.when(this.objectMapper.readValue(MESSAGE, PromoBundlingActivatedEvent.class))
      .thenReturn(promoBundlingActivatedEvent);
    listener.onDomainEventConsumed(MESSAGE);
    Mockito.verify(this.objectMapper).readValue(MESSAGE, PromoBundlingActivatedEvent.class);
  }

  @Test
  public void onDomainEventConsumed_itemSkuEmptyTest() throws Exception {
    promoBundlingActivatedEvent.setMainItemSku(StringUtils.EMPTY);
    Mockito.when(this.objectMapper.readValue(MESSAGE, PromoBundlingActivatedEvent.class))
      .thenReturn(promoBundlingActivatedEvent);
    listener.onDomainEventConsumed(MESSAGE);
    Mockito.verify(this.objectMapper).readValue(MESSAGE, PromoBundlingActivatedEvent.class);
  }

  @Test
  public void onDomainEventConsumed_eventTypeTest() throws Exception {
    promoBundlingActivatedEvent.setPromoBundlingType(StringUtils.EMPTY);
    Mockito.when(this.objectMapper.readValue(MESSAGE, PromoBundlingActivatedEvent.class))
      .thenReturn(promoBundlingActivatedEvent);
    listener.onDomainEventConsumed(MESSAGE);
    Mockito.verify(this.objectMapper).readValue(MESSAGE, PromoBundlingActivatedEvent.class);
  }
}
