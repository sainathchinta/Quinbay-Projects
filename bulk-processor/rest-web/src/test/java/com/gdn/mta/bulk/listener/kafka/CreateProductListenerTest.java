package com.gdn.mta.bulk.listener.kafka;

import com.gdn.common.exception.ApplicationException;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.mta.bulk.config.KafkaTopicProperties;
import com.gdn.mta.bulk.service.GeneralProcessorService;
import com.gdn.partners.bulk.util.Constant;
import com.gdn.x.mta.rest.web.request.ProductApiRequest;

/**
 * Created by hardikbohra on 29/05/18.
 */
public class CreateProductListenerTest {

  @InjectMocks
  private CreateProductListener listener;

  @Mock
  private GeneralProcessorService<ProductApiRequest, Void, Void> productLevel3ApiProcessorServiceBean;

  @Mock
  private ObjectMapper objectMapper;

  @Mock
  private KafkaTopicProperties kafkaTopicProperties;

  private ProductApiRequest productApiRequest;

  @BeforeEach
  public void init() {
    MockitoAnnotations.initMocks(this);
  }

  @AfterEach
  public void tearDown() {
    Mockito.verifyNoMoreInteractions(productLevel3ApiProcessorServiceBean);
    Mockito.verifyNoMoreInteractions(objectMapper);
    Mockito.verifyNoMoreInteractions(kafkaTopicProperties);
  }

  @Test
  public void testListen() throws Exception {
    productApiRequest = new ProductApiRequest();
    Mockito.when(objectMapper.readValue(Constant.CLIENT_ID, ProductApiRequest.class)).thenReturn(productApiRequest);
    listener.onDomainEventConsumed(Constant.CLIENT_ID);
    Mockito.verify(productLevel3ApiProcessorServiceBean).process(productApiRequest);
    Mockito.verify(objectMapper).readValue(Constant.CLIENT_ID, ProductApiRequest.class);
    Mockito.verify(kafkaTopicProperties).getCreateProductEvent();
  }

  @Test
  public void testListenWhenError() throws Exception {
    productApiRequest = new ProductApiRequest();
    Mockito.when(objectMapper.readValue(Constant.CLIENT_ID, ProductApiRequest.class)).thenReturn(productApiRequest);
    Mockito.doThrow(new Exception()).when(productLevel3ApiProcessorServiceBean).process(productApiRequest);
    try {
      Assertions.assertThrows(Exception.class,
          () -> listener.onDomainEventConsumed(Constant.CLIENT_ID));
    } finally {
      Mockito.verify(productLevel3ApiProcessorServiceBean).process(productApiRequest);
      Mockito.verify(objectMapper).readValue(Constant.CLIENT_ID, ProductApiRequest.class);
      Mockito.verify(kafkaTopicProperties).getCreateProductEvent();
    }
  }
}
