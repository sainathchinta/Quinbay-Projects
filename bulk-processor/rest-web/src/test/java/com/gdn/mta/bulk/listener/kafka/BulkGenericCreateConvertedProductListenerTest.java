package com.gdn.mta.bulk.listener.kafka;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.common.exception.ApplicationException;
import com.gdn.mta.bulk.config.KafkaTopicProperties;
import com.gdn.mta.bulk.entity.BulkCreateProductEventModel;
import com.gdn.mta.bulk.service.BulkGenericProcessorService;
import org.junit.jupiter.api.BeforeEach;
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
public class BulkGenericCreateConvertedProductListenerTest {

  @Mock
  private BulkGenericProcessorService bulkGenericProcessorService;

  @Mock
  private ObjectMapper objectMapper;

  @Mock
  private KafkaTopicProperties kafkaTopicProperties;

  @InjectMocks
  private BulkGenericCreateConvertedProductListener listener;

  private static final String STORE_ID = "store-123";
  private static final String BULK_PROCESS_CODE = "BPC-123";
  private static final String TOPIC = "bulk-generic-create-converted-product-topic";

  private BulkCreateProductEventModel eventModel;
  private String message;

  @BeforeEach
  void setUp() {
    eventModel = BulkCreateProductEventModel.builder()
      .storeId(STORE_ID)
      .bulkProcessCode(BULK_PROCESS_CODE)
      .build();

    message = "{\"storeId\":\"" + STORE_ID + "\",\"bulkProcessCode\":\"" + BULK_PROCESS_CODE + "\"}";
  }


  @Test
  public void testOnDomainEventConsumedForPriority1_Success() throws Exception {
    // Given
    when(kafkaTopicProperties.getBulkGenericCreateProductForConvertedUpload()).thenReturn(TOPIC);
    when(objectMapper.readValue(message, BulkCreateProductEventModel.class))
      .thenReturn(eventModel);
    doNothing().when(bulkGenericProcessorService).processBulkGenericEvent(eventModel);

    // When
    listener.onDomainEventConsumedForPriority1(message);

    // Then
    verify(objectMapper).readValue(message, BulkCreateProductEventModel.class);
    verify(bulkGenericProcessorService).processBulkGenericEvent(eventModel);
  }

  @Test
  public void testOnDomainEventConsumedForPriority1_SuccessStoreIdEmpty() throws Exception {
    // Given
    message = "{\"storeId\":\"" + "\",\"bulkProcessCode\":\"" + BULK_PROCESS_CODE + "\"}";
    when(kafkaTopicProperties.getBulkGenericCreateProductForConvertedUpload()).thenReturn(TOPIC);
    when(objectMapper.readValue(message, BulkCreateProductEventModel.class))
      .thenReturn(eventModel);
    eventModel.setStoreId("");
    doNothing().when(bulkGenericProcessorService).processBulkGenericEvent(eventModel);

    // When
    listener.onDomainEventConsumedForPriority1(message);

    // Then
    verify(objectMapper).readValue(message, BulkCreateProductEventModel.class);
    verify(bulkGenericProcessorService).processBulkGenericEvent(eventModel);
  }

  @Test
  public void testOnDomainEventConsumedForPriority1_WhenProcessingFails() throws Exception {
    // Given
    when(kafkaTopicProperties.getBulkGenericCreateProductForConvertedUpload()).thenReturn(TOPIC);
    when(objectMapper.readValue(message, BulkCreateProductEventModel.class))
      .thenReturn(eventModel);
    doThrow(new RuntimeException("Processing failed"))
      .when(bulkGenericProcessorService).processBulkGenericEvent(eventModel);

    // When & Then
    assertThrows(ApplicationException.class,
      () -> listener.onDomainEventConsumedForPriority1(message));
  }
}
