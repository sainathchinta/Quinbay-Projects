package com.gdn.mta.product.service.domainevent;

import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verifyNoMoreInteractions;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.mta.domain.event.modal.TerminatedSellerSkuCleanupEvent;
import com.gdn.mta.product.service.ProductServiceWrapper;
import com.gdn.mta.product.service.config.KafkaTopicProperties;

public class TerminatedSellerSkuCleanupListenerTest {

  @InjectMocks
  TerminatedSellerSkuCleanupListener terminatedSellerSkuCleanupListener;

  @Mock
  private KafkaTopicProperties kafkaTopicProperties;

  @Mock
  private ProductServiceWrapper productServiceWrapper;

  @Mock
  private ObjectMapper objectMapper;

  private TerminatedSellerSkuCleanupEvent terminatedSellerSkuCleanupEvent;

  private static final String SELLER_CODE = "sellerCode";
  private static final String PRODUCT_CODE = "productCode";
  @BeforeEach
  public void init() throws Exception {
    MockitoAnnotations.initMocks(this);
    terminatedSellerSkuCleanupEvent = new TerminatedSellerSkuCleanupEvent();
    terminatedSellerSkuCleanupEvent.setSellerCode(SELLER_CODE);
    terminatedSellerSkuCleanupEvent.setProductCode(PRODUCT_CODE);
  }


  @AfterEach
  public void tearDown(){
    verifyNoMoreInteractions(productServiceWrapper);
    verifyNoMoreInteractions(objectMapper);
  }

  @Test
  public void onDomainEventConsumedTest() throws Exception {
    String message = objectMapper.writeValueAsString(terminatedSellerSkuCleanupEvent);
    Mockito.when(objectMapper.readValue(message, TerminatedSellerSkuCleanupEvent.class))
        .thenReturn(this.terminatedSellerSkuCleanupEvent);
    this.terminatedSellerSkuCleanupListener.onDomainEventConsumed(message);
    Mockito.verify(objectMapper).readValue(message, TerminatedSellerSkuCleanupEvent.class);
    Mockito.verify(objectMapper).writeValueAsString(terminatedSellerSkuCleanupEvent);
    Mockito.verify(productServiceWrapper).terminatedSellerSkuCleanup(Mockito.anyString(), Mockito.anyString());
  }

  @Test
  public void onDomainEventConsumedExceptionTest() throws Exception {
    String message = objectMapper.writeValueAsString(terminatedSellerSkuCleanupEvent);
    Mockito.when(objectMapper.readValue(message, TerminatedSellerSkuCleanupEvent.class))
        .thenThrow(new NullPointerException());
    terminatedSellerSkuCleanupListener.onDomainEventConsumed(message);
    Mockito.verify(objectMapper).readValue(message, TerminatedSellerSkuCleanupEvent.class);
    Mockito.verify(objectMapper).writeValueAsString(terminatedSellerSkuCleanupEvent);
    Mockito.verify(productServiceWrapper, times(0))
        .terminatedSellerSkuCleanup(terminatedSellerSkuCleanupEvent.getProductCode(),
            terminatedSellerSkuCleanupEvent.getSellerCode());
  }

}
