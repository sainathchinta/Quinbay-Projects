package com.gdn.x.mta.distributiontask.inbound.impl;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.x.mta.distributiontask.domain.event.model.DeleteOriginalImageEventModel;
import com.gdn.x.mta.distributiontask.inbound.config.KafkaTopicPropertiesConsumer;
import com.gdn.x.mta.distributiontask.service.api.FileStorageService;
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
public class DeleteOriginalImageEventListenerTest {

  private static final String JSON =
      "{\"locationPath\": \"path\"}";

  private static final String LOCATION_PATH = "locationPath";

  @InjectMocks
  private DeleteOriginalImageEventListener deleteOriginalImageEventListener;

  @Mock
  private ObjectMapper objectMapper;

  @Mock
  private FileStorageService fileStorageService;

  @Mock
  private KafkaTopicPropertiesConsumer kafkaTopicPropertiesConsumer;

  private DeleteOriginalImageEventModel deleteOriginalImageEventModel;

  @BeforeEach
  void setUp() {
    deleteOriginalImageEventModel = new DeleteOriginalImageEventModel();
    deleteOriginalImageEventModel.setLocationPath(LOCATION_PATH);
  }

  @AfterEach
  void tearDown() {
    Mockito.verifyNoMoreInteractions(objectMapper, fileStorageService, kafkaTopicPropertiesConsumer);
  }

  @Test
  void onDomainEventConsumedTest() throws Exception {
    Mockito.when(objectMapper.readValue(JSON, DeleteOriginalImageEventModel.class))
        .thenReturn(deleteOriginalImageEventModel);
    deleteOriginalImageEventListener.onDomainEventConsumed(JSON);
    Mockito.verify(objectMapper).readValue(JSON, DeleteOriginalImageEventModel.class);
    Mockito.verify(fileStorageService).deleteOriginalImages(LOCATION_PATH);
    Mockito.verify(kafkaTopicPropertiesConsumer).getDeleteOriginalImageEvent();
  }

  @Test
  void onDomainEventConsumedExceptionTest() throws Exception {
    Mockito.when(objectMapper.readValue(JSON, DeleteOriginalImageEventModel.class))
        .thenThrow(JsonParseException.class);
    try {
      deleteOriginalImageEventListener.onDomainEventConsumed(JSON);
    } finally {
      Mockito.verify(objectMapper).readValue(JSON, DeleteOriginalImageEventModel.class);
      Mockito.verify(kafkaTopicPropertiesConsumer, Mockito.times(2)).getDeleteOriginalImageEvent();
    }
  }

}
