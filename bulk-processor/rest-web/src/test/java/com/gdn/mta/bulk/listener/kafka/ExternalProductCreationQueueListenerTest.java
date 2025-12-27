package com.gdn.mta.bulk.listener.kafka;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.mta.bulk.config.KafkaTopicProperties;
import com.gdn.mta.bulk.dto.BulkProcessExternalUploadRequest;
import com.gdn.mta.bulk.service.BulkProcessService;
import com.gdn.mta.bulk.service.ExternalProductCreationService;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.*;

import static org.mockito.Mockito.*;

public class ExternalProductCreationQueueListenerTest {

  @InjectMocks
  private ExternalProductCreationQueueListener listener;

  @Mock
  private ExternalProductCreationService externalProductCreationService;

  @Mock
  private BulkProcessService bulkProcessService;

  @Mock
  private ObjectMapper objectMapper;

  @Mock
  private KafkaTopicProperties kafkaTopicProperties;

  private final String topic = "test-topic";

  @BeforeEach
  void setUp() {
    MockitoAnnotations.initMocks(this);
    when(kafkaTopicProperties.getBulkExternalCreateEvent()).thenReturn(topic);
  }

  @Test
  void testOnDomainEventConsumed_happyPath() throws Exception {
    String message = "{\"requestId\":\"req-1\",\"bulkProcessCode\":\"BULK-123\"}";
    BulkProcessExternalUploadRequest request = new BulkProcessExternalUploadRequest();

    when(objectMapper.readValue(message, BulkProcessExternalUploadRequest.class)).thenReturn(request);

    listener.onDomainEventConsumed(message);

    verify(objectMapper).readValue(message, BulkProcessExternalUploadRequest.class);
    verify(externalProductCreationService).process(request);
    verifyNoMoreInteractions(bulkProcessService);
  }

  @Test
  void testOnDomainEventConsumed_processThrowsException() throws Exception {
    String message = "{\"requestId\":\"req-1\",\"bulkProcessCode\":\"BULK-123\",\"storeId\":\"store-1\"}";
    BulkProcessExternalUploadRequest request = new BulkProcessExternalUploadRequest();
    request.setRequestId("req-1");
    request.setBulkProcessCode("BULK-123");
    request.setStoreId("store-1");

    when(objectMapper.readValue(message, BulkProcessExternalUploadRequest.class)).thenReturn(request);
    doThrow(new RuntimeException("Processing failed")).when(externalProductCreationService).process(request);

    listener.onDomainEventConsumed(message);

    verify(externalProductCreationService).process(request);
    verify(bulkProcessService).abortBulkProcess("store-1", "BULK-123");
  }

  @Test
  void testOnDomainEventConsumed_invalidJson() throws Exception {
    String invalidMessage = "invalid json";
    when(objectMapper.readValue(invalidMessage, BulkProcessExternalUploadRequest.class))
      .thenThrow(new JsonProcessingException("Invalid JSON") {});
    listener.onDomainEventConsumed(invalidMessage);
    // process and abortBulkProcess should never be called
    verify(externalProductCreationService, never()).process(any());
    verify(bulkProcessService, never()).abortBulkProcess(anyString(), anyString());
  }
}

