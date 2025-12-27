package com.gdn.x.mta.distributiontask.inbound.impl;

import static org.mockito.MockitoAnnotations.initMocks;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.mta.domain.event.modal.ProductActionRetryEvent;
import com.gdn.x.mta.distributiontask.model.Constants;
import com.gdn.x.mta.distributiontask.service.api.ProductActionRetryService;

public class ProductActionRetryEventListenerTest {

  private static final String PRODUCT_CODE = "code";

  private ObjectMapper mapper;
  private ProductActionRetryEvent productActionRetryEvent;

  @InjectMocks
  private ProductActionRetryEventListener productActionRetryEventListener;

  @Mock
  private ObjectMapper objectMapper;

  @Mock
  private ProductActionRetryService productActionRetryService;

  @BeforeEach
  public void setUp() throws Exception {
    initMocks(this);
    mapper = new ObjectMapper();
    productActionRetryEvent = new ProductActionRetryEvent();
    productActionRetryEvent.setProductCode(PRODUCT_CODE);
    productActionRetryEvent.setStoreId(Constants.DEFAULT_STORE_ID);
  }

  @AfterEach
  public void tearDown() throws Exception {
    Mockito.verifyNoMoreInteractions(objectMapper);
    Mockito.verifyNoMoreInteractions(productActionRetryService);
  }

  @Test
   void onDomainEventConsumedTest() throws Exception {
    Mockito
        .when(objectMapper.readValue(mapper.writeValueAsString(productActionRetryEvent), ProductActionRetryEvent.class))
        .thenReturn(productActionRetryEvent);
    productActionRetryEventListener.onDomainEventConsumed(mapper.writeValueAsString(productActionRetryEvent));
    Mockito.verify(productActionRetryService).upsertProductActionRetry(productActionRetryEvent);
    Mockito.verify(objectMapper)
        .readValue(mapper.writeValueAsString(productActionRetryEvent), ProductActionRetryEvent.class);
  }

  @Test
   void onDomainEventConsumedExceptionTest() throws Exception {
    Mockito.doThrow(RuntimeException.class).when(productActionRetryService).upsertProductActionRetry(productActionRetryEvent);
    Mockito
        .when(objectMapper.readValue(mapper.writeValueAsString(productActionRetryEvent), ProductActionRetryEvent.class))
        .thenReturn(productActionRetryEvent);
    productActionRetryEventListener.onDomainEventConsumed(mapper.writeValueAsString(productActionRetryEvent));
    Mockito.verify(productActionRetryService).upsertProductActionRetry(productActionRetryEvent);
    Mockito.verify(objectMapper)
        .readValue(mapper.writeValueAsString(productActionRetryEvent), ProductActionRetryEvent.class);
  }
}
