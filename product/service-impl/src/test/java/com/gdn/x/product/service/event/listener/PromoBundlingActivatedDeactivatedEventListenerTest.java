package com.gdn.x.product.service.event.listener;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.partners.product.pricing.model.enums.PromoBundlingEventType;
import com.gdn.partners.product.pricing.streaming.model.promo.bundling.PromoBundlingActivatedDeactivatedEventModel;
import com.gdn.x.product.service.api.ItemService;

public class PromoBundlingActivatedDeactivatedEventListenerTest {

  private static final String MESSAGE = "message";

  @Mock
  private ItemService itemService;

  @Mock
  private ObjectMapper objectMapper;

  @InjectMocks
  private PromoBundlingActivatedDeactivatedEventListener listener;

  private PromoBundlingActivatedDeactivatedEventModel promoBundlingActivatedDeactivatedEventModel;

  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.openMocks(this);
    promoBundlingActivatedDeactivatedEventModel = PromoBundlingActivatedDeactivatedEventModel.builder()
        .promoBundlingEventType(PromoBundlingEventType.PROMO_STARTED).build();
  }

  @AfterEach
  public void tearDown() throws Exception {
    Mockito.verifyNoMoreInteractions(this.itemService);
    Mockito.verifyNoMoreInteractions(this.objectMapper);
  }

  @Test
  public void onDomainEventConsumedTest() throws Exception {
    Mockito.when(this.objectMapper.readValue(MESSAGE, PromoBundlingActivatedDeactivatedEventModel.class))
        .thenReturn(promoBundlingActivatedDeactivatedEventModel);
    listener.onDomainEventConsumed(MESSAGE);
    Mockito.verify(this.itemService)
        .activeDeactivatePromoBundlingInItemPickupPointByPPCode(promoBundlingActivatedDeactivatedEventModel);
    Mockito.verify(this.objectMapper).readValue(MESSAGE, PromoBundlingActivatedDeactivatedEventModel.class);
  }

  @Test
  public void onDomainEventConsumedMPPEnabledTest() throws Exception {
    Mockito.when(this.objectMapper.readValue(MESSAGE, PromoBundlingActivatedDeactivatedEventModel.class))
        .thenReturn(promoBundlingActivatedDeactivatedEventModel);
    listener.onDomainEventConsumed(MESSAGE);
    Mockito.verify(this.itemService)
        .activeDeactivatePromoBundlingInItemPickupPointByPPCode(promoBundlingActivatedDeactivatedEventModel);
    Mockito.verify(this.objectMapper).readValue(MESSAGE, PromoBundlingActivatedDeactivatedEventModel.class);
  }

  @Test
  public void onDomainEventConsumedChangeTypeTest() throws Exception {
    promoBundlingActivatedDeactivatedEventModel.setPromoBundlingEventType(
        PromoBundlingEventType.PROMO_ACTIVE_COMPLEMENTARY_AVAILABLE);
    Mockito.when(this.objectMapper.readValue(MESSAGE, PromoBundlingActivatedDeactivatedEventModel.class))
        .thenReturn(promoBundlingActivatedDeactivatedEventModel);
    listener.onDomainEventConsumed(MESSAGE);
    Mockito.verify(this.objectMapper).readValue(MESSAGE, PromoBundlingActivatedDeactivatedEventModel.class);
  }

  @Test
  public void onDomainEventConsumedExceptionTest() throws Exception {
    Mockito.when(this.objectMapper.readValue(MESSAGE, PromoBundlingActivatedDeactivatedEventModel.class))
        .thenReturn(promoBundlingActivatedDeactivatedEventModel);
    Mockito.doThrow(RuntimeException.class).when(this.itemService)
        .activeDeactivatePromoBundlingInItemPickupPointByPPCode(promoBundlingActivatedDeactivatedEventModel);
    listener.onDomainEventConsumed(MESSAGE);
    Mockito.verify(this.itemService).activeDeactivatePromoBundlingInItemPickupPointByPPCode(promoBundlingActivatedDeactivatedEventModel);
    Mockito.verify(this.objectMapper).readValue(MESSAGE, PromoBundlingActivatedDeactivatedEventModel.class);
  }
}