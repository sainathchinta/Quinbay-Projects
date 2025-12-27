package com.gdn.mta.bulk.listener.kafka;

import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import java.util.UUID;
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
import com.gdn.mta.bulk.models.download.BulkDownloadRequest;
import com.gdn.mta.bulk.service.download.BulkProcessDownloadService;
import com.gdn.partners.bulk.util.Constant;

/**
 * Created by hardikbohra on 29/05/18.
 */
public class BulkDownloadAllListenerTest {

  @InjectMocks
  private BulkDownloadAllListener listener;

  @Mock
  private BulkProcessDownloadService bulkProcessDownloadService;

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
    Mockito.verifyNoMoreInteractions(bulkProcessDownloadService);
    Mockito.verifyNoMoreInteractions(objectMapper);
    Mockito.verifyNoMoreInteractions(kafkaTopicProperties);
  }

  @Test
  public void bulkDownloadProductWhenNull() throws Exception {
    listener.onDomainEventConsumed("");
    verify(bulkProcessDownloadService, Mockito.never()).downloadAll(Mockito.any());
    Mockito.verify(objectMapper).readValue(Mockito.anyString(), Mockito.eq((BulkDownloadRequest.class)));
    Mockito.verify(kafkaTopicProperties).getBulkDownloadAllEvent();
  }

  @Test
  public void listenWhenSuccesss() throws Exception {
    BulkDownloadRequest message = new BulkDownloadRequest();
    message.setRequestId(UUID.randomUUID().toString());
    Mockito.when(objectMapper.readValue(Constant.CLIENT_ID, BulkDownloadRequest.class)).thenReturn(message);
    listener.onDomainEventConsumed(Constant.CLIENT_ID);
    verify(bulkProcessDownloadService).downloadAll(Mockito.any());
    Mockito.verify(objectMapper).readValue(Constant.CLIENT_ID, BulkDownloadRequest.class);
    Mockito.verify(kafkaTopicProperties).getBulkDownloadAllEvent();
  }

  @Test
  public void listenWhenError() throws Exception {
    Mockito.doThrow(new Exception()).when(bulkProcessDownloadService).downloadAll(Mockito.any(BulkDownloadRequest
        .class));
    Mockito.when(objectMapper.readValue(Constant.CLIENT_ID, BulkDownloadRequest.class))
        .thenReturn(new BulkDownloadRequest());
    try {
      Assertions.assertThrows(Exception.class,
          () -> listener.onDomainEventConsumed(Constant.CLIENT_ID));
    } finally {
      verify(bulkProcessDownloadService).downloadAll(Mockito.any());
      Mockito.verify(objectMapper).readValue(Constant.CLIENT_ID, BulkDownloadRequest.class);
      Mockito.verify(kafkaTopicProperties).getBulkDownloadAllEvent();
    }
  }

}
