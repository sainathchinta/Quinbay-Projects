package com.gdn.mta.bulk.listener.kafka;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.common.exception.ApplicationException;
import com.gdn.mta.bulk.config.KafkaTopicProperties;
import com.gdn.mta.bulk.entity.BulkUpdateEventModel;
import com.gdn.mta.bulk.service.BulkBasicInfoUpdateService;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
public class BulkBasicInfoUpdateListenerServiceBeanTest {

  @Mock
  private KafkaTopicProperties kafkaTopicProperties;

  @Mock
  private BulkBasicInfoUpdateService bulkBasicInfoUpdateService;

  @Mock
  private ObjectMapper objectMapper;

  @InjectMocks
  private BulkBasicInfoUpdateListenerServiceBean listenerService;

  private static final String STORE_ID = "store-123";
  private static final String BULK_PROCESS_CODE = "BPC-123";
  private static final String TOPIC = "bulk-basic-info-update-topic";
  private static final String PRIORITY1_TOPIC = "bulk-basic-info-update-priority1-topic";
  private static final String PRIORITY2_TOPIC = "bulk-basic-info-update-priority2-topic";

  private static BulkUpdateEventModel bulkUpdateEventModel;
  private static String message;

  @BeforeAll
  public static void setUp() throws Exception {
    // Setup test data
    bulkUpdateEventModel =
        BulkUpdateEventModel.builder().storeId(STORE_ID).bulkProcessCode(BULK_PROCESS_CODE).build();

    message =
        "{\"storeId\":\"" + STORE_ID + "\",\"bulkProcessCode\":\"" + BULK_PROCESS_CODE + "\"}";
  }

  @Test
  public void testOnBasicInfoUpdateEvent_Success() throws Exception {
    // Given
    when(objectMapper.readValue(message, BulkUpdateEventModel.class)).thenReturn(
        bulkUpdateEventModel);
    when(kafkaTopicProperties.getBulkBasicInfoUpdateEvent()).thenReturn(TOPIC);
    doNothing().when(bulkBasicInfoUpdateService).processBulkBasicInfoUpdate(bulkUpdateEventModel);

    // When
    listenerService.onBasicInfoUpdateEvent(message);

    // Then
    verify(objectMapper).readValue(message, BulkUpdateEventModel.class);
    verify(bulkBasicInfoUpdateService).processBulkBasicInfoUpdate(bulkUpdateEventModel);
  }

  @Test
  public void testBulkBasicInfoUpdatePriority1Event_Success() throws Exception {
    // Given
    when(objectMapper.readValue(message, BulkUpdateEventModel.class)).thenReturn(
        bulkUpdateEventModel);
    when(kafkaTopicProperties.getBulkBasicInfoUpdatePriority1Event()).thenReturn(PRIORITY1_TOPIC);
    doNothing().when(bulkBasicInfoUpdateService).processBulkBasicInfoUpdate(bulkUpdateEventModel);

    // When
    listenerService.bulkBasicInfoUpdatePriority1Event(message);

    // Then
    verify(objectMapper).readValue(message, BulkUpdateEventModel.class);
    verify(bulkBasicInfoUpdateService).processBulkBasicInfoUpdate(bulkUpdateEventModel);
  }

  @Test
  public void testBulkBasicInfoUpdatePriority2Event_Success() throws Exception {
    // Given
    when(objectMapper.readValue(message, BulkUpdateEventModel.class)).thenReturn(
        bulkUpdateEventModel);
    when(kafkaTopicProperties.getBulkBasicInfoUpdatePriority2Event()).thenReturn(PRIORITY2_TOPIC);
    doNothing().when(bulkBasicInfoUpdateService).processBulkBasicInfoUpdate(bulkUpdateEventModel);

    // When
    listenerService.bulkBasicInfoUpdatePriority2Event(message);

    // Then
    verify(objectMapper).readValue(message, BulkUpdateEventModel.class);
    verify(bulkBasicInfoUpdateService).processBulkBasicInfoUpdate(bulkUpdateEventModel);
  }

  @Test
  public void testOnBasicInfoUpdateEvent_WhenServiceThrowsException() throws Exception {
    // Given
    when(objectMapper.readValue(message, BulkUpdateEventModel.class)).thenReturn(
        bulkUpdateEventModel);
    when(kafkaTopicProperties.getBulkBasicInfoUpdateEvent()).thenReturn(TOPIC);
    doThrow(new RuntimeException("Processing failed")).when(bulkBasicInfoUpdateService)
        .processBulkBasicInfoUpdate(bulkUpdateEventModel);

    // When & Then
    assertThrows(ApplicationException.class, () -> listenerService.onBasicInfoUpdateEvent(message));
  }

  @Test
  public void testBulkBasicInfoUpdatePriority1Event_WhenServiceThrowsException() throws Exception {
    // Given
    when(objectMapper.readValue(message, BulkUpdateEventModel.class)).thenReturn(
        bulkUpdateEventModel);
    when(kafkaTopicProperties.getBulkBasicInfoUpdatePriority1Event()).thenReturn(PRIORITY1_TOPIC);
    doThrow(new RuntimeException("Processing failed")).when(bulkBasicInfoUpdateService)
        .processBulkBasicInfoUpdate(bulkUpdateEventModel);

    // When & Then
    assertThrows(ApplicationException.class,
        () -> listenerService.bulkBasicInfoUpdatePriority1Event(message));
  }

  @Test
  public void testBulkBasicInfoUpdatePriority2Event_WhenServiceThrowsException() throws Exception {
    // Given
    when(objectMapper.readValue(message, BulkUpdateEventModel.class)).thenReturn(
        bulkUpdateEventModel);
    when(kafkaTopicProperties.getBulkBasicInfoUpdatePriority2Event()).thenReturn(PRIORITY2_TOPIC);
    doThrow(new RuntimeException("Processing failed")).when(bulkBasicInfoUpdateService)
        .processBulkBasicInfoUpdate(bulkUpdateEventModel);
    assertThrows(ApplicationException.class,
        () -> listenerService.bulkBasicInfoUpdatePriority2Event(message));
  }
} 