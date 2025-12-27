package com.gdn.mta.bulk.listener.kafka;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.mta.bulk.config.KafkaTopicProperties;
import com.gdn.mta.bulk.dto.BulkBasicInfoRequest;
import com.gdn.mta.bulk.service.BulkBasicInfoUpdateService;

public class BulkUploadBasicInfoListenerTest {

  private static final String MESSAGE = "test-message";
  private static final String TOPIC_1 = "topic-1";
  private static final String TOPIC_2 = "topic-2";
  private static final String USER_NAME = "test-user";

  @InjectMocks
  private BulkUploadBasicInfoListener listener;

  @Mock
  private ObjectMapper objectMapper;

  @Mock
  private KafkaTopicProperties kafkaTopicProperties;

  @Mock
  private BulkBasicInfoUpdateService bulkBasicInfoUpdateService;

  @Mock
  private BulkBasicInfoRequest bulkBasicInfoRequest;

  @BeforeEach()
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);

    when(kafkaTopicProperties.getBulkBasicInfoUploadPriority1Event()).thenReturn(TOPIC_1);
    when(kafkaTopicProperties.getBulkBasicInfoUploadPriority2Event()).thenReturn(TOPIC_2);
    when(objectMapper.readValue(MESSAGE, BulkBasicInfoRequest.class)).thenReturn(bulkBasicInfoRequest);
    when(bulkBasicInfoRequest.getUpdatedBy()).thenReturn(USER_NAME);
  }

  @AfterEach
  public void tearDown() {
    Mockito.verifyNoMoreInteractions(objectMapper, kafkaTopicProperties, bulkBasicInfoUpdateService);
  }

  @Test
  public void onDomainEventConsumed_Success() throws Exception {
    listener.onDomainEventConsumed(MESSAGE);

    verify(kafkaTopicProperties).getBulkBasicInfoUploadPriority1Event();
    verify(objectMapper).readValue(MESSAGE, BulkBasicInfoRequest.class);
    verify(bulkBasicInfoRequest).getUpdatedBy();
    verify(bulkBasicInfoUpdateService).processBulkUpdate(bulkBasicInfoRequest);
    verify(bulkBasicInfoRequest).getBulkProcessCode();
  }

  @Test
  public void onDomainEventConsumed_WhenProcessingFails() throws Exception {
    doThrow(new RuntimeException("Processing failed"))
        .when(bulkBasicInfoUpdateService).processBulkUpdate(any());

    listener.onDomainEventConsumed(MESSAGE);

    verify(kafkaTopicProperties).getBulkBasicInfoUploadPriority1Event();
    verify(objectMapper).readValue(MESSAGE, BulkBasicInfoRequest.class);
    verify(bulkBasicInfoRequest).getUpdatedBy();
    verify(bulkBasicInfoUpdateService).processBulkUpdate(bulkBasicInfoRequest);
    verify(bulkBasicInfoRequest).getBulkProcessCode();
  }

  @Test
  public void onDomainEventConsumedForPriority1_Success() throws Exception {
    listener.onDomainEventConsumedForPriority1(MESSAGE);

    verify(kafkaTopicProperties).getBulkBasicInfoUploadPriority2Event();
    verify(objectMapper).readValue(MESSAGE, BulkBasicInfoRequest.class);
    verify(bulkBasicInfoRequest).getUpdatedBy();
    verify(bulkBasicInfoUpdateService).processBulkUpdate(bulkBasicInfoRequest);
    verify(bulkBasicInfoRequest).getBulkProcessCode();
  }

  @Test
  public void onDomainEventConsumedForPriority1_WhenProcessingFails() throws Exception {
    doThrow(new RuntimeException("Processing failed"))
        .when(bulkBasicInfoUpdateService).processBulkUpdate(any());

    listener.onDomainEventConsumedForPriority1(MESSAGE);

    verify(kafkaTopicProperties).getBulkBasicInfoUploadPriority2Event();
    verify(objectMapper).readValue(MESSAGE, BulkBasicInfoRequest.class);
    verify(bulkBasicInfoRequest).getUpdatedBy();
    verify(bulkBasicInfoUpdateService).processBulkUpdate(bulkBasicInfoRequest);
    verify(bulkBasicInfoRequest).getBulkProcessCode();
  }
}