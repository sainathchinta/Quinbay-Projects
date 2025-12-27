package com.gdn.mta.bulk.listener.kafka;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.mta.bulk.config.KafkaTopicProperties;
import com.gdn.mta.bulk.models.BulkDataDeletionModel;
import com.gdn.mta.bulk.service.BulkProcessService;

public class BulkDataDeletionListenerTest {

  private static final String STORE_ID = "10001";
  private static final String BULK_PROCESS_CODE = "bulkProcessCode";

  @InjectMocks
  private BulkDataDeletionListener bulkDataDeletionListener;

  @Mock
  private ObjectMapper objectMapper;

  @Mock
  private KafkaTopicProperties kafkaTopicProperties;

  @Mock
  private BulkProcessService bulkProcessService;

  private String json;
  private ObjectMapper mapper = new ObjectMapper();
  private BulkDataDeletionModel bulkDataDeletionModel;

  @BeforeEach
  public void init() throws JsonProcessingException {
    MockitoAnnotations.initMocks(this);
    bulkDataDeletionModel = new BulkDataDeletionModel(STORE_ID, BULK_PROCESS_CODE);
    json = mapper.writeValueAsString(bulkDataDeletionModel);
  }

  @AfterEach
  public void tearDown() {
    Mockito.verifyNoMoreInteractions(objectMapper, bulkProcessService, kafkaTopicProperties);
  }

  @Test
  public void onDomainEventConsumedTest() throws JsonProcessingException {
    Mockito.when(objectMapper.readValue(json, BulkDataDeletionModel.class)).thenReturn(bulkDataDeletionModel);
    Mockito.doNothing().when(bulkProcessService).deleteBulkProcessDataByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE);

    bulkDataDeletionListener.onDomainEventConsumed(json);

    Mockito.verify(objectMapper).readValue(json, BulkDataDeletionModel.class);
    Mockito.verify(bulkProcessService).deleteBulkProcessDataByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE);
    Mockito.verify(kafkaTopicProperties).getBulkDataDeletionEvent();
  }

  @Test
  public void onDomainEventConsumedExceptionTest() throws JsonProcessingException {
    Mockito.when(objectMapper.readValue(json, BulkDataDeletionModel.class)).thenThrow(JsonProcessingException.class);

    bulkDataDeletionListener.onDomainEventConsumed(json);

    Mockito.verify(objectMapper).readValue(json, BulkDataDeletionModel.class);
    Mockito.verify(kafkaTopicProperties, Mockito.times(2)).getBulkDataDeletionEvent();
  }
}
