package com.gdn.aggregate.platform.module.product.listener.listener.direct.sivaBoth;

import com.gdn.aggregate.platform.module.product.listener.constants.Topics;
import com.gdn.aggregate.platform.module.product.listener.service.helper.ListenerService;
import io.micrometer.tracing.Tracer;
import org.apache.kafka.clients.consumer.ConsumerRecord;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.verify;

class PickupPointDataCollectionUpdateQueueHighVolumeListenerTest {

  private static final String TEST_TOPIC = Topics.NEW_PICKUP_POINT_UPSERT_EVENT_HIGH_VOLUME;
  private static final int TEST_PARTITION = 0;
  private static final long TEST_OFFSET = 0L;
  private static final String TEST_KEY = "pickup-point-key";
  private static final String TEST_VALUE = "pickup-point-value";

  @Mock
  private ListenerService listenerService;

  @Mock
  private Tracer tracer;

  @InjectMocks
  private PickupPointDataCollectionUpdateQueueHighVolumeListener pickupPointListener;

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

  @Test
  void testProcessNewItemDataUpdateEvent() {
    ConsumerRecord<String, String> testRecord = createTestConsumerRecord();
    pickupPointListener.processNewItemDataUpdateEvent(testRecord);
    verify(listenerService).onPickupPointCombinedUpsertEvent(eq(testRecord));
  }

  @Test
  void testProcessNewItemDataUpdateEvent_WhenExceptionThrown() {
    ConsumerRecord<String, String> testRecord = createTestConsumerRecord();
    RuntimeException testException = new RuntimeException("Test exception");
    org.mockito.Mockito.doThrow(testException)
        .when(listenerService)
        .onPickupPointCombinedUpsertEvent(testRecord);

    org.junit.jupiter.api.Assertions.assertThrows(
        RuntimeException.class,
        () -> pickupPointListener.processNewItemDataUpdateEvent(testRecord)
    );

    verify(listenerService).onPickupPointCombinedUpsertEvent(eq(testRecord));
  }
}
