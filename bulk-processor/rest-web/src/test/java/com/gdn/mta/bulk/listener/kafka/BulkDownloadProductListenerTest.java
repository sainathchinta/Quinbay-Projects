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
import com.gdn.mta.bulk.entity.BulkDownloadQueue;
import com.gdn.mta.bulk.service.BulkDownloadService;
import com.gdn.partners.bulk.util.Constant;

/**
 * Created by hardikbohra on 29/05/18.
 */
public class BulkDownloadProductListenerTest {

  @InjectMocks
  private BulkDownloadProductListener productListener;

  @Mock
  private BulkDownloadService bulkDownloadService;

  @Mock
  private ObjectMapper objectMapper;

  @Mock
  private KafkaTopicProperties kafkaTopicProperties;

  @BeforeEach
  public void init() {
    MockitoAnnotations.initMocks(this);
  }

  @AfterEach
  public void tearDown() {
    Mockito.verifyNoMoreInteractions(bulkDownloadService);
    Mockito.verifyNoMoreInteractions(objectMapper);
    Mockito.verifyNoMoreInteractions(kafkaTopicProperties);
  }

  @Test
  public void onDomainEventConsumedTest() throws Exception {
    BulkDownloadQueue bulkDownloadQueue = new BulkDownloadQueue();
    Mockito.when(objectMapper.readValue(Constant.CLIENT_ID, BulkDownloadQueue.class)).thenReturn(bulkDownloadQueue);
    productListener.onDomainEventConsumed(Constant.CLIENT_ID);
    Mockito.verify(bulkDownloadService)
        .postProcess(Mockito.any(BulkDownloadQueue.class));
    Mockito.verify(objectMapper).readValue(Constant.CLIENT_ID, BulkDownloadQueue.class);
    Mockito.verify(kafkaTopicProperties).getBulkProductDownloadEvent();
  }

  @Test
  public void onDomainEventConsumedTest_handleException() throws Exception {
    BulkDownloadQueue bulkDownloadQueue = new BulkDownloadQueue();
    Mockito.when(objectMapper.readValue(Constant.CLIENT_ID, BulkDownloadQueue.class)).thenReturn(bulkDownloadQueue);
    try {
      Mockito.doThrow(new Exception()).when(bulkDownloadService).postProcess(bulkDownloadQueue);

      Assertions.assertThrows(Exception.class,
          () -> productListener.onDomainEventConsumed(Constant.CLIENT_ID));
    } finally {
      Mockito.verify(bulkDownloadService).postProcess(Mockito.any(BulkDownloadQueue.class));
      Mockito.verify(objectMapper).readValue(Constant.CLIENT_ID, BulkDownloadQueue.class);
      Mockito.verify(kafkaTopicProperties).getBulkProductDownloadEvent();
    }
  }
}
