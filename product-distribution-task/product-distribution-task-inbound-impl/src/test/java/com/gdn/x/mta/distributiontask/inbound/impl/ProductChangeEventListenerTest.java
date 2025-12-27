package com.gdn.x.mta.distributiontask.inbound.impl;


import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.x.mta.distributiontask.domain.event.model.ProductChange;
import com.gdn.x.mta.distributiontask.inbound.config.KafkaTopicPropertiesConsumer;
import com.gdn.x.mta.distributiontask.service.api.IprWrapperService;
import com.gdn.x.mta.distributiontask.service.api.ProductWrapperService;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;

@ExtendWith(MockitoExtension.class)
class ProductChangeEventListenerTest {
  private static final String PRODUCT_SKU = "productSku";
  private static final String EVENT = "event";

  @InjectMocks
  private ProductChangeEventListener productChangeEventListener;

  @Mock
  private ObjectMapper objectMapper;

  @Mock
  private IprWrapperService iprService;

  @Mock
  private KafkaTopicPropertiesConsumer kafkaTopicPropertiesConsumer;

  @Mock
  private ProductWrapperService productWrapperService;

  private ProductChange productChange;

  @BeforeEach
  public void setUp() {
    productChange = new ProductChange();
    productChange.setProductSku(PRODUCT_SKU);
  }

  @AfterEach
  public void tearDown() {
    Mockito.verifyNoMoreInteractions(objectMapper, iprService, kafkaTopicPropertiesConsumer, productWrapperService);
  }

  @Test
  void addProductToIPREventListenerTest() throws Exception {
    Mockito.when(objectMapper.readValue(Mockito.anyString(), Mockito.eq(ProductChange.class)))
      .thenReturn(productChange);
    Mockito.when(kafkaTopicPropertiesConsumer.getProductChangeEvent()).thenReturn(EVENT);
    productChangeEventListener.onDomainEventConsumed(PRODUCT_SKU);
    Mockito.verify(kafkaTopicPropertiesConsumer).getProductChangeEvent();
    Mockito.verify(objectMapper).readValue(Mockito.anyString(), Mockito.eq(ProductChange.class));
    Mockito.verify(iprService).updateProductOnStateChange(productChange);
    Mockito.verify(productWrapperService).updateDistributionMappingStatusOnChange(productChange);
  }

  @Test
  void addProductToIPREventListenerExceptionTest() throws JsonProcessingException {
    Mockito.when(kafkaTopicPropertiesConsumer.getProductChangeEvent()).thenReturn(EVENT);
    Mockito.when(objectMapper.readValue(Mockito.anyString(), Mockito.eq(ProductChange.class)))
      .thenThrow(new ApplicationRuntimeException());
    productChangeEventListener.onDomainEventConsumed(PRODUCT_SKU);
    Mockito.verify(kafkaTopicPropertiesConsumer, Mockito.times(2)).getProductChangeEvent();
    Mockito.verify(objectMapper).readValue(Mockito.anyString(), Mockito.eq(ProductChange.class));
  }
}
