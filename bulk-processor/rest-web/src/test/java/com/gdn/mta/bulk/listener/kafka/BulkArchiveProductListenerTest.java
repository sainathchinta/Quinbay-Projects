package com.gdn.mta.bulk.listener.kafka;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.mta.bulk.config.KafkaTopicProperties;
import com.gdn.mta.bulk.entity.BulkUpdateEventModel;
import com.gdn.mta.bulk.service.BulkArchiveService;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

public class BulkArchiveProductListenerTest {

  private static final String STORE_ID = "storeId";
  private static final String MESSAGE = "message";

  private BulkUpdateEventModel bulkUpdateEventModel = new BulkUpdateEventModel();

  @Mock
  private BulkArchiveService bulkArchiveService;

  @Mock
  private ObjectMapper objectMapper;

  @Mock
  private KafkaTopicProperties kafkaTopicProperties;

  @InjectMocks
  private BulkArchiveProductListener bulkArchiveProductListener;

  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);
    bulkUpdateEventModel.setStoreId(STORE_ID);
  }

  @AfterEach
  public void tearDown() throws Exception {
    Mockito.verifyNoMoreInteractions(bulkArchiveService);
    Mockito.verifyNoMoreInteractions(kafkaTopicProperties);
  }

  @Test
  public void onDomainEventConsumedTest() throws Exception {
    Mockito.when(this.objectMapper.readValue(MESSAGE, BulkUpdateEventModel.class))
      .thenReturn(bulkUpdateEventModel);
    bulkArchiveProductListener.onDomainEventConsumed(MESSAGE);
    Mockito.verify(this.objectMapper).readValue(MESSAGE, BulkUpdateEventModel.class);
    Mockito.verify(this.bulkArchiveService).processArchiveEvent(bulkUpdateEventModel);
    Mockito.verify(kafkaTopicProperties).getBulkArchiveProductRows();
  }

  @Test
  public void onDomainEventConsumed_exceptionTest() throws Exception {
    Mockito.when(this.objectMapper.readValue(MESSAGE, BulkUpdateEventModel.class))
      .thenReturn(bulkUpdateEventModel);
    Mockito.doThrow(Exception.class).when(this.bulkArchiveService)
      .processArchiveEvent(bulkUpdateEventModel);
    bulkArchiveProductListener.onDomainEventConsumed(MESSAGE);
    Mockito.verify(this.objectMapper).readValue(MESSAGE, BulkUpdateEventModel.class);
    Mockito.verify(this.bulkArchiveService).processArchiveEvent(bulkUpdateEventModel);
    Mockito.verify(kafkaTopicProperties).getBulkArchiveProductRows();
  }
}