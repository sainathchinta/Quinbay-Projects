package com.gdn.mta.product.service.domainevent;

import static org.mockito.Mockito.verify;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.mta.product.service.ProductServiceWrapper;
import com.gdn.x.product.domain.event.model.WholesalePriceActivatedOrDeactivatedEvent;

public class WholesalePriceActivatedOrDeactivatedSubscriberTest {

  private static final String MERCHANT_CODE = "merchantCode";
  private static final String ITEM_SKU = "itemSku";
  private WholesalePriceActivatedOrDeactivatedEvent event;
  private ObjectMapper mapper;

  @InjectMocks
  private WholesalePriceActivatedOrDeactivatedSubscriber wholesalePriceActivatedOrDeactivatedSubscriber;

  @Mock
  private ProductServiceWrapper productServiceWrapper;

  @Mock
  private ObjectMapper objectMapper;

  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);
    event = new WholesalePriceActivatedOrDeactivatedEvent(ITEM_SKU, true, MERCHANT_CODE);
    mapper = new ObjectMapper();
  }

  @AfterEach
  public void tearDown() throws Exception {
    Mockito.verifyNoMoreInteractions(productServiceWrapper, objectMapper);
  }

  @Test
  public void onDomainEventConsumedActivatedTrue() throws Exception {
    String message = mapper.writeValueAsString(this.event);
    Mockito.when(objectMapper.readValue(message, WholesalePriceActivatedOrDeactivatedEvent.class))
        .thenReturn(this.event);
    this.wholesalePriceActivatedOrDeactivatedSubscriber.onDomainEventConsumed(message);

    verify(objectMapper).readValue(message, WholesalePriceActivatedOrDeactivatedEvent.class);
    Mockito.verify(productServiceWrapper)
        .updateProductHistoryOnWholesaleChangesByScheduler(MERCHANT_CODE, ITEM_SKU, true);
  }

  @Test
  public void onDomainEventConsumedActivatedFalse() throws Exception {
    event.setWholesalePriceActivated(false);
    String message = mapper.writeValueAsString(this.event);
    Mockito.when(objectMapper.readValue(message, WholesalePriceActivatedOrDeactivatedEvent.class))
        .thenReturn(this.event);
    this.wholesalePriceActivatedOrDeactivatedSubscriber.onDomainEventConsumed(message);
    verify(objectMapper).readValue(message, WholesalePriceActivatedOrDeactivatedEvent.class);
    Mockito.verify(productServiceWrapper)
        .updateProductHistoryOnWholesaleChangesByScheduler(MERCHANT_CODE, ITEM_SKU, false);
  }
}