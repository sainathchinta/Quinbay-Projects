package com.gdn.x.mta.distributiontask.inbound.impl;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.x.mta.distributiontask.domain.event.model.IPRHistoryEventModel;
import com.gdn.x.mta.distributiontask.inbound.config.KafkaTopicPropertiesConsumer;
import com.gdn.x.mta.distributiontask.service.api.IprService;
import org.bson.json.JsonParseException;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;

@ExtendWith(MockitoExtension.class)
class IprHistoryListenerTest {

  private static final String JSON = "{}";
  private static final String PRODUCT_SKU = "ProductSku";
  private static final String EVENT = "event";
  private static final String STORE_ID = "10001";

  @InjectMocks
  private IprHistoryListener iprHistoryListener;

  @Mock
  private ObjectMapper objectMapper;

  @Mock
  private IprService iprService;

  @Mock
  private KafkaTopicPropertiesConsumer kafkaTopicPropertiesConsumer;

  private IPRHistoryEventModel iprHistoryEventModel;

  @BeforeEach
  void setUp() {
    iprHistoryEventModel = new IPRHistoryEventModel();
    iprHistoryEventModel.setProductSku(PRODUCT_SKU);
  }

  @AfterEach
  void tearDown() {
    Mockito.verifyNoMoreInteractions(objectMapper, iprService, kafkaTopicPropertiesConsumer);
  }

  @Test
  void onDomainEventConsumedTest() throws Exception {
    Mockito.when(objectMapper.readValue(JSON, IPRHistoryEventModel.class))
      .thenReturn(iprHistoryEventModel);
    iprHistoryListener.onDomainEventConsumed(JSON);
    Mockito.verify(objectMapper).readValue(JSON, IPRHistoryEventModel.class);
    Mockito.verify(iprService).updateIprHistoryForProduct(STORE_ID, iprHistoryEventModel);
    Mockito.verify(kafkaTopicPropertiesConsumer).getPublishHistoryForIprEvent();
  }

  @Test
  void onDomainEventConsumedExceptionTest() throws Exception {
    Mockito.when(objectMapper.readValue(JSON, IPRHistoryEventModel.class))
      .thenThrow(JsonParseException.class);
    try {
      iprHistoryListener.onDomainEventConsumed(JSON);
    } finally {
      Mockito.verify(objectMapper).readValue(JSON, IPRHistoryEventModel.class);
      Mockito.verify(kafkaTopicPropertiesConsumer, Mockito.times(2)).getPublishHistoryForIprEvent();
    }
  }
}
