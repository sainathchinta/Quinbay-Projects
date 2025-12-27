package com.gdn.x.mta.distributiontask.inbound.impl;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.x.mta.distributiontask.domain.event.model.PDTProductsPermanentDeleteEventModel;
import com.gdn.x.mta.distributiontask.inbound.config.KafkaTopicPropertiesConsumer;
import com.gdn.x.mta.distributiontask.service.api.ProductWrapperService;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.mockito.junit.jupiter.MockitoExtension;

@ExtendWith(MockitoExtension.class)
class PDTProductsPermanentDeleteEventListenerTest {
  @Mock
  private ObjectMapper objectMapper;

  @Mock
  private ProductWrapperService productWrapperService;

  @Mock
  private KafkaTopicPropertiesConsumer kafkaTopicPropertiesConsumer;

  @InjectMocks
  private PDTProductsPermanentDeleteEventListener listener;

  private PDTProductsPermanentDeleteEventModel pdtProductsPermanentDeleteEventModel;
  private ObjectMapper mapper;

  @BeforeEach
  public void setUp() {
    mapper = new ObjectMapper();
    pdtProductsPermanentDeleteEventModel = new PDTProductsPermanentDeleteEventModel();
    pdtProductsPermanentDeleteEventModel.setProductCode("12345");
    pdtProductsPermanentDeleteEventModel.setSellerCode("S123");
    MockitoAnnotations.openMocks(this);
  }

  @AfterEach
  public void tearDown() {
    Mockito.verifyNoMoreInteractions(objectMapper);
    Mockito.verifyNoMoreInteractions(productWrapperService);
    Mockito.verifyNoMoreInteractions(kafkaTopicPropertiesConsumer);
  }

  @Test
  void testOnDomainEventConsumed_Success() throws Exception {
    String message = mapper.writeValueAsString(pdtProductsPermanentDeleteEventModel);
    Mockito.when(objectMapper.readValue(message, PDTProductsPermanentDeleteEventModel.class))
      .thenReturn(pdtProductsPermanentDeleteEventModel);
    listener.onDomainEventConsumed(message);
    Mockito.verify(productWrapperService).processProductsPermanentDelete("12345", "S123");
    Mockito.verify(objectMapper).readValue(message, PDTProductsPermanentDeleteEventModel.class);
    Mockito.verify(kafkaTopicPropertiesConsumer).getProductAnalyticsPermanentDeleteEvent();
  }

  @Test
  void testOnDomainEventConsumed_Exception() throws Exception {
    String message = mapper.writeValueAsString(pdtProductsPermanentDeleteEventModel);
    Mockito.when(objectMapper.readValue(message, PDTProductsPermanentDeleteEventModel.class))
      .thenThrow(new RuntimeException("Error parsing JSON"));
    listener.onDomainEventConsumed(message);
    Mockito.verify(productWrapperService, Mockito.never())
      .processProductsPermanentDelete(Mockito.any(), Mockito.any());
    Mockito.verify(objectMapper).readValue(message, PDTProductsPermanentDeleteEventModel.class);
    Mockito.verify(kafkaTopicPropertiesConsumer).getProductAnalyticsPermanentDeleteEvent();
  }

  @Test
  void testOnDomainEventConsumed_ProductCode_null() throws Exception {
    pdtProductsPermanentDeleteEventModel.setProductCode(null);
    String message = mapper.writeValueAsString(pdtProductsPermanentDeleteEventModel);
    Mockito.when(objectMapper.readValue(message, PDTProductsPermanentDeleteEventModel.class))
      .thenReturn(pdtProductsPermanentDeleteEventModel);
    listener.onDomainEventConsumed(message);
    Mockito.verify(productWrapperService, Mockito.never())
      .processProductsPermanentDelete(Mockito.any(), Mockito.any());
    Mockito.verify(objectMapper).readValue(message, PDTProductsPermanentDeleteEventModel.class);
    Mockito.verify(kafkaTopicPropertiesConsumer).getProductAnalyticsPermanentDeleteEvent();
  }
}
