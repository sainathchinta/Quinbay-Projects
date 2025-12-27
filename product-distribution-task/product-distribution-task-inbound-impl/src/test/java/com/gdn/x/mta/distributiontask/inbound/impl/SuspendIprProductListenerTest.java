package com.gdn.x.mta.distributiontask.inbound.impl;

import com.gdn.x.mta.distributiontask.inbound.config.KafkaTopicPropertiesConsumer;
import com.gdn.x.mta.distributiontask.service.api.IprWrapperService;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

class SuspendIprProductListenerTest {

  private static final String PRODUCT_SKU = "productSku";
  private static final String EVENT = "event";

  @InjectMocks
  private SuspendIprProductListener suspendIprProductListener;
  @Mock
  private KafkaTopicPropertiesConsumer kafkaTopicPropertiesConsumer;

  @Mock
  private IprWrapperService iprWrapperService;

  @BeforeEach
  public void setUp() {
    MockitoAnnotations.initMocks(this);
  }

  @AfterEach
  public void tearDown() {
    Mockito.verifyNoMoreInteractions(iprWrapperService);
    Mockito.verifyNoMoreInteractions(kafkaTopicPropertiesConsumer);
  }

  @Test
  void addProductToIPREventListenerTest() throws Exception {
    Mockito.when(kafkaTopicPropertiesConsumer.getSuspendIprProductEvent()).thenReturn(EVENT);
    suspendIprProductListener.onDomainEventConsumed(PRODUCT_SKU);
    Mockito.verify(iprWrapperService).suspendEvidenceRequestedProduct(PRODUCT_SKU);
    Mockito.verify(kafkaTopicPropertiesConsumer).getSuspendIprProductEvent();
  }

  @Test
  void addProductToIPREventListenerExceptionTest() throws Exception {
    Mockito.when(kafkaTopicPropertiesConsumer.getSuspendIprProductEvent()).thenReturn(EVENT);
    Mockito.doThrow(new Exception()).when(iprWrapperService)
      .suspendEvidenceRequestedProduct(PRODUCT_SKU);
    suspendIprProductListener.onDomainEventConsumed(PRODUCT_SKU);
    Mockito.verify(iprWrapperService).suspendEvidenceRequestedProduct(PRODUCT_SKU);
    Mockito.verify(kafkaTopicPropertiesConsumer, Mockito.times(2)).getSuspendIprProductEvent();
  }
}
