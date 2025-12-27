package com.gdn.aggregate.platform.module.product.listener.listener.direct.sivaBoth;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.gdn.aggregate.platform.module.product.listener.constants.Topics;
import com.gdn.aggregate.platform.module.product.listener.service.helper.ListenerService;
import org.apache.kafka.clients.consumer.ConsumerRecord;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;

import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.verify;

class SivaItemCollectionUpdateQueueHighVolumeListenerTest {

  private static final String TEST_TOPIC = Topics.NEW_SIVA_ITEM_COLLECTION_UPDATE_EVENT_HIGH_VOLUME;
  private static final int TEST_PARTITION = 0;
  private static final long TEST_OFFSET = 0L;
  private static final String TEST_KEY = "siva-item-key";
  private static final String TEST_VALUE = "siva-item-value";

  @Mock
  private ListenerService listenerService;

  @InjectMocks
  private SivaItemCollectionUpdateQueueHighVolumeListener sivaItemListener;

  private AutoCloseable closeable;

  @BeforeEach
  void setUp() {
    closeable = MockitoAnnotations.openMocks(this);
  }

  @AfterEach
  void tearDown() throws Exception {
    closeable.close();
  }

  private ConsumerRecord<String, String> createTestConsumerRecord() {
    return new ConsumerRecord<>(
      TEST_TOPIC,
      TEST_PARTITION,
      TEST_OFFSET,
      TEST_KEY,
      TEST_VALUE
    );
  }

  private List<ConsumerRecord<String, String>> createTestConsumerRecords(int count) {
    List<ConsumerRecord<String, String>> records = new ArrayList<>();
    for (int i = 0; i < count; i++) {
      records.add(createSingleConsumerRecord(TEST_KEY + i, TEST_VALUE + i, i));
    }
    return records;
  }

  private ConsumerRecord<String, String> createSingleConsumerRecord(String key, String value, long offset) {
    return new ConsumerRecord<>(TEST_TOPIC, TEST_PARTITION, offset, key, value);
  }

  @Test
  void testProcessHighVolumeItemDataUpdateEvent() throws JsonProcessingException {
    List<ConsumerRecord<String, String>> testConsumerRecords = createTestConsumerRecords(1);
    sivaItemListener.processHighVolumeItemDataUpdateEvent(testConsumerRecords);
    verify(listenerService).processSivaItemUpdateQueueEventBatch(eq(testConsumerRecords));
  }

  @Test
  void testProcessHighVolumeItemDataUpdateEvent_WhenJsonProcessingExceptionThrown() throws JsonProcessingException {
    List<ConsumerRecord<String, String>> testConsumerRecords = createTestConsumerRecords(1);
    RuntimeException testException = new RuntimeException("Test JSON processing exception") {};
    doThrow(testException)
        .when(listenerService)
        .processSivaItemUpdateQueueEventBatch(testConsumerRecords);

    org.junit.jupiter.api.Assertions.assertThrows(
        RuntimeException.class,
        () -> sivaItemListener.processHighVolumeItemDataUpdateEvent(testConsumerRecords)
    );

    verify(listenerService).processSivaItemUpdateQueueEventBatch(eq(testConsumerRecords));
  }

  @Test
  void testProcessHighVolumeItemDataUpdateEvent_WhenRuntimeExceptionThrown() throws JsonProcessingException {
    List<ConsumerRecord<String, String>> testConsumerRecords = createTestConsumerRecords(1);
    RuntimeException testException = new RuntimeException("Test runtime exception");
    doThrow(testException)
        .when(listenerService)
        .processSivaItemUpdateQueueEventBatch(testConsumerRecords);

    org.junit.jupiter.api.Assertions.assertThrows(
        RuntimeException.class,
        () -> sivaItemListener.processHighVolumeItemDataUpdateEvent(testConsumerRecords)
    );

    verify(listenerService).processSivaItemUpdateQueueEventBatch(eq(testConsumerRecords));
  }
}
