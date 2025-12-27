package com.gdn.mta.bulk.listener.kafka;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.mta.bulk.config.KafkaTopicProperties;
import com.gdn.mta.bulk.dto.product.ProductV2Request;
import com.gdn.mta.bulk.service.GeneralProcessorService;
import com.gdn.partners.bulk.util.Constant;

public class CreateProductV2ListenerTest {

  @InjectMocks
  private CreateProductV2Listener createProductV2Listener;

  @Mock
  private GeneralProcessorService<ProductV2Request, Void, Void> createProductV2ApiService;

  @Mock
  private ObjectMapper objectMapper;

  @Mock
  private KafkaTopicProperties kafkaTopicProperties;
  
  @BeforeEach
  public void initialize() throws Exception {
    MockitoAnnotations.initMocks(this);
  }
  
  @AfterEach
  public void tearDown() throws Exception {
    Mockito.verifyNoMoreInteractions(createProductV2ApiService, kafkaTopicProperties);
    Mockito.verifyNoMoreInteractions(objectMapper);
  }
  
  @Test
  public void onDomainEventConsumed_HappyFlow_Success() throws Exception {
    ProductV2Request request = new ProductV2Request();
    Mockito.when(objectMapper.readValue(Constant.CLIENT_ID, ProductV2Request.class)).thenReturn(request);
    Mockito.when(createProductV2ApiService.process(request)).thenReturn(null);
    createProductV2Listener.onDomainEventConsumed(Constant.CLIENT_ID);
    Mockito.verify(createProductV2ApiService).process(request);
    Mockito.verify(objectMapper).readValue(Constant.CLIENT_ID, ProductV2Request.class);
    Mockito.verify(kafkaTopicProperties).getBulkApiCreateProductV2Event();
  }
  
  @Test
  public void onDomainEventConsumed_Error_Failed() throws Exception {
    ProductV2Request request = new ProductV2Request();
    Mockito.when(objectMapper.readValue(Constant.CLIENT_ID, ProductV2Request.class)).thenReturn(request);
    Mockito.when(createProductV2ApiService.process(request)).thenThrow(Exception.class);
    createProductV2Listener.onDomainEventConsumed(Constant.CLIENT_ID);
    Mockito.verify(createProductV2ApiService).process(request);
    Mockito.verify(objectMapper).readValue(Constant.CLIENT_ID, ProductV2Request.class);
    Mockito.verify(kafkaTopicProperties).getBulkApiCreateProductV2Event();
  }
  
}
