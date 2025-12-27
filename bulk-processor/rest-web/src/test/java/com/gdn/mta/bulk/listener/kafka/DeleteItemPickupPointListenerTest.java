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
import com.gdn.mta.bulk.entity.BulkUpdateEventModel;
import com.gdn.mta.bulk.service.PickupPointDeleteService;

public class DeleteItemPickupPointListenerTest {

  private static final String STORE_ID = "10001";
  private static final String BULK_PROCESS_CODE = "bulkProcessCode";

  @InjectMocks
  private DeleteItemPickupPointListener deleteItemPickupPointListener;

  @Mock
  private ObjectMapper objectMapper;

  @Mock
  private PickupPointDeleteService pickupPointDeleteService;

  @Mock
  private KafkaTopicProperties kafkaTopicProperties;

  private BulkUpdateEventModel bulkUpdateEventModel;
  private ObjectMapper mapper = new ObjectMapper();

  @BeforeEach
  public void setUp() {
    MockitoAnnotations.initMocks(this);
    bulkUpdateEventModel = new BulkUpdateEventModel();
    bulkUpdateEventModel.setStoreId(STORE_ID);
    bulkUpdateEventModel.setBulkProcessCode(BULK_PROCESS_CODE);
    bulkUpdateEventModel.setBusinessPartnerCode(BULK_PROCESS_CODE);
  }

  @AfterEach
  public void tearDown() throws Exception {
    Mockito.verifyNoMoreInteractions(objectMapper);
    Mockito.verifyNoMoreInteractions(pickupPointDeleteService);
    Mockito.verifyNoMoreInteractions(kafkaTopicProperties);
  }

  @Test
  public void onDomainEventConsumedTest() throws Exception {
    Mockito.when(objectMapper.readValue(mapper.writeValueAsString(bulkUpdateEventModel), BulkUpdateEventModel.class))
        .thenReturn(bulkUpdateEventModel);
    deleteItemPickupPointListener.onDomainEventConsumed(mapper.writeValueAsString(bulkUpdateEventModel));
    Mockito.verify(objectMapper).readValue(mapper.writeValueAsString(bulkUpdateEventModel), BulkUpdateEventModel.class);
    Mockito.verify(pickupPointDeleteService).processDeleteItemPickupPointEvent(bulkUpdateEventModel);
    Mockito.verify(kafkaTopicProperties).getBulkDeleteItemPickupPoint();
  }

  @Test
  public void onDomainEventConsumedExceptionTest() throws Exception {
    Mockito.when(objectMapper.readValue(mapper.writeValueAsString(bulkUpdateEventModel), BulkUpdateEventModel.class))
        .thenReturn(bulkUpdateEventModel);
    Mockito.doThrow(Exception.class).when(pickupPointDeleteService).processDeleteItemPickupPointEvent(bulkUpdateEventModel);
    deleteItemPickupPointListener.onDomainEventConsumed(mapper.writeValueAsString(bulkUpdateEventModel));
    Mockito.verify(objectMapper).readValue(mapper.writeValueAsString(bulkUpdateEventModel), BulkUpdateEventModel.class);
    Mockito.verify(pickupPointDeleteService).processDeleteItemPickupPointEvent(bulkUpdateEventModel);
    Mockito.verify(kafkaTopicProperties).getBulkDeleteItemPickupPoint();
  }
}