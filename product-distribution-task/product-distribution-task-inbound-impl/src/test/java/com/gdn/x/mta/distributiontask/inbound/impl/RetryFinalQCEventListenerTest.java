package com.gdn.x.mta.distributiontask.inbound.impl;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.x.mta.distributiontask.domain.event.model.RetryFinalQCEventModel;
import com.gdn.x.mta.distributiontask.service.api.ProductService;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

public class RetryFinalQCEventListenerTest {
  private static final String JSON = "{}";
  private static final String PRODUCT_CODE = "productCode";

  @InjectMocks
  RetryFinalQCEventListener retryFinalQCEventListener;

  @Mock
  ObjectMapper objectMapper;

  @Mock
  ProductService productService;

  @BeforeEach
  public void setUp() {
    MockitoAnnotations.openMocks(this);
  }

  @AfterEach
  public void tearDown() {
    Mockito.verifyNoMoreInteractions(objectMapper, productService);
  }

  @Test
   void retryFinalQCEventListenerTest() throws JsonProcessingException {
    RetryFinalQCEventModel retryFinalQCEventModel = new RetryFinalQCEventModel();
    retryFinalQCEventModel.setProductCode(PRODUCT_CODE);
    Mockito.when(objectMapper.readValue(JSON, RetryFinalQCEventModel.class))
        .thenReturn(retryFinalQCEventModel);
    retryFinalQCEventListener.onDomainEventConsumed(JSON);
    Mockito.verify(objectMapper).readValue(JSON, RetryFinalQCEventModel.class);
    Mockito.verify(productService).retryFinalQCProducts(PRODUCT_CODE);
  }

  @Test
   void retryFinalQCEventListenerExceptionTest() throws JsonProcessingException {
    RetryFinalQCEventModel retryFinalQCEventModel = new RetryFinalQCEventModel();
    retryFinalQCEventModel.setProductCode(PRODUCT_CODE);
    Mockito.when(objectMapper.readValue(JSON, RetryFinalQCEventModel.class))
        .thenThrow(new ApplicationRuntimeException());
    retryFinalQCEventListener.onDomainEventConsumed(JSON);
    Mockito.verify(objectMapper).readValue(JSON, RetryFinalQCEventModel.class);
  }
}
