package com.gdn.x.product.service.event.listener;

import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;
import static org.mockito.MockitoAnnotations.openMocks;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.partners.product.pricing.streaming.model.promo.bundling.PromoBundlingDeactivatedEvent;
import com.gdn.x.product.model.entity.Item;
import com.gdn.x.product.service.api.ItemService;

public class PromoBundlingDeactivatedEventListenerTest {

  private final String ITEM_SKU = "item-sku";

  private final String PROMO_BUNDLING_TYPE = "promo-bundling-type";

  private final String STORE_ID = "store-id";

  private static final String MESSAGE = "message";

  @InjectMocks
  private PromoBundlingDeactivatedEventListener listener;

  @Mock
  private ItemService itemService;

  @Mock
  private ObjectMapper objectMapper;

  @Test
  public void onDomainEventConsumed_FailedBecauseServiceThrowException() throws Exception {
    when(this.itemService.removeActivePromoBundling(this.STORE_ID, this.ITEM_SKU,
        this.PROMO_BUNDLING_TYPE)).thenThrow(new RuntimeException());
    Mockito.when(this.objectMapper.readValue(MESSAGE, PromoBundlingDeactivatedEvent.class))
      .thenReturn(this.constructPromoBundlingDeactivated());
    this.listener.onDomainEventConsumed(MESSAGE);
    Mockito.verify(this.objectMapper).readValue(MESSAGE, PromoBundlingDeactivatedEvent.class);
    verify(this.itemService).removeActivePromoBundling(this.STORE_ID, this.ITEM_SKU,
        this.PROMO_BUNDLING_TYPE);
  }

  private PromoBundlingDeactivatedEvent constructPromoBundlingDeactivated() {
    PromoBundlingDeactivatedEvent promoBundlingDeactivated = new PromoBundlingDeactivatedEvent();
    promoBundlingDeactivated.setStoreId(this.STORE_ID);
    promoBundlingDeactivated.setSku(this.ITEM_SKU);
    promoBundlingDeactivated.setPromoBundlingType(this.PROMO_BUNDLING_TYPE);
    return promoBundlingDeactivated;
  }

  @Test
  public void onDomainEventConsumed_Success() throws Exception {
    when(this.itemService.removeActivePromoBundling(this.STORE_ID, this.ITEM_SKU,
        this.PROMO_BUNDLING_TYPE)).thenReturn(this.constructItem());

    Mockito.when(this.objectMapper.readValue(MESSAGE, PromoBundlingDeactivatedEvent.class))
      .thenReturn(this.constructPromoBundlingDeactivated());
    this.listener.onDomainEventConsumed(MESSAGE);
    Mockito.verify(this.objectMapper).readValue(MESSAGE, PromoBundlingDeactivatedEvent.class);

    verify(this.itemService).removeActivePromoBundling(this.STORE_ID, this.ITEM_SKU,
        this.PROMO_BUNDLING_TYPE);
  }

  private Item constructItem() {
    return new Item();
  }

  @Test
  public void onDomainEventConsumed_SuccessWithInvalidMessage() throws Exception {
    Mockito.when(this.objectMapper.readValue(MESSAGE, PromoBundlingDeactivatedEvent.class))
      .thenReturn(this.constructInvalidPromoBundlingDeactivated());
    this.listener.onDomainEventConsumed(MESSAGE);
    Mockito.verify(this.objectMapper).readValue(MESSAGE, PromoBundlingDeactivatedEvent.class);
  }

  private PromoBundlingDeactivatedEvent constructInvalidPromoBundlingDeactivated() {
    return new PromoBundlingDeactivatedEvent();
  }

  @BeforeEach
  public void setUp() throws Exception {
    openMocks(this);
  }

  @AfterEach
  public void tearDown() throws Exception {
    verifyNoMoreInteractions(this.itemService);
  }
}
