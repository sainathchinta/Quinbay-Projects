package com.gdn.aggregate.platform.module.product.listener.listener.direct.sivaBoth;

import com.gdn.aggregate.platform.module.product.listener.configurations.TraceHelper;
import com.gdn.aggregate.platform.module.product.listener.constants.Topics;
import com.gdn.aggregate.platform.module.product.listener.service.helper.ListenerService;
import io.micrometer.tracing.Tracer;
import org.apache.kafka.clients.consumer.ConsumerRecord;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockedStatic;
import org.mockito.MockitoAnnotations;

import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

class RawItemDataCollectionUpdateQueueHighVolumeListenerTest {

  private static final String TEST_TOPIC = Topics.NEW_RAW_ITEM_COLLECTION_UPDATE_EVENT_HIGH_VOLUME;
  private static final int TEST_PARTITION = 0;
  private static final long TEST_OFFSET = 0L;
  private static final String TEST_KEY = "raw-item-key";
  private static final String TEST_VALUE = "raw-item-value";
  private static final String TEST_TRACE_ID = "test-trace-id";

  @Mock
  private ListenerService listenerService;

  @Mock
  private Tracer tracer;

  @InjectMocks
  private RawItemDataCollectionUpdateQueueHighVolumeListener rawItemListener;

  private AutoCloseable closeable;
  private MockedStatic<TraceHelper> traceHelperMock;

  @BeforeEach
  void setUp() {
    closeable = MockitoAnnotations.openMocks(this);
    traceHelperMock = mockStatic(TraceHelper.class);
  }

  @AfterEach
  void tearDown() throws Exception {
    traceHelperMock.close();
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
  void testProcessHighVolumeRawItemDataUpdateEvent() {
    ConsumerRecord<String, String> testRecord = createTestConsumerRecord();
    when(TraceHelper.getTraceId(tracer)).thenReturn(TEST_TRACE_ID);

    rawItemListener.processHighVolumeRawItemDataUpdateEvent(testRecord);

    verify(listenerService).onNewItemEventV2(
      eq(testRecord), 
      eq(Topics.GROUP_ID_SIVA_BOTH_BY_ITEM_HIGH_VOLUME), 
      eq(TEST_TRACE_ID)
    );
  }

  @Test
  void testProcessHighVolumeRawItemDataUpdateEvent_WhenExceptionThrown() {
    ConsumerRecord<String, String> testRecord = createTestConsumerRecord();
    RuntimeException testException = new RuntimeException("Test exception");
    when(TraceHelper.getTraceId(tracer)).thenReturn(TEST_TRACE_ID);
    
    org.mockito.Mockito.doThrow(testException)
        .when(listenerService)
        .onNewItemEventV2(testRecord, Topics.GROUP_ID_SIVA_BOTH_BY_ITEM_HIGH_VOLUME, TEST_TRACE_ID);

    org.junit.jupiter.api.Assertions.assertThrows(
        RuntimeException.class,
        () -> rawItemListener.processHighVolumeRawItemDataUpdateEvent(testRecord)
    );

    verify(listenerService).onNewItemEventV2(
      eq(testRecord), 
      eq(Topics.GROUP_ID_SIVA_BOTH_BY_ITEM_HIGH_VOLUME), 
      eq(TEST_TRACE_ID)
    );
  }
}
