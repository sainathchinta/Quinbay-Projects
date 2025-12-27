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

class RawProductUpdateCombinedEventHighVolumeListenerTest {

  private static final String TEST_TOPIC = Topics.RAW_PRODUCT_COMBINED_UPSERT_HIGH_VOLUME;
  private static final int TEST_PARTITION = 0;
  private static final long TEST_OFFSET = 0L;
  private static final String TEST_KEY = "raw-product-key";
  private static final String TEST_VALUE = "raw-product-value";
  private static final String TEST_TRACE_ID = "test-trace-id";

  @Mock
  private ListenerService listenerService;

  @Mock
  private Tracer tracer;

  @InjectMocks
  private RawProductUpdateCombinedEventHighVolumeListener rawProductListener;

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
  void testOnHighVolumeRawProductUpdateCombinedEvent() throws Exception {
    ConsumerRecord<String, String> testRecord = createTestConsumerRecord();
    when(TraceHelper.getTraceId(tracer)).thenReturn(TEST_TRACE_ID);

    rawProductListener.onHighVolumeRawProductUpdateCombinedEvent(testRecord);

    verify(listenerService).saveProductAndRelated(eq(testRecord), eq(TEST_TRACE_ID));
  }

  @Test
  void testOnHighVolumeRawProductUpdateCombinedEvent_WhenExceptionThrown() throws Exception {
    ConsumerRecord<String, String> testRecord = createTestConsumerRecord();
    RuntimeException testException = new RuntimeException("Test exception");
    when(TraceHelper.getTraceId(tracer)).thenReturn(TEST_TRACE_ID);
    
    org.mockito.Mockito.doThrow(testException)
        .when(listenerService)
        .saveProductAndRelated(testRecord, TEST_TRACE_ID);

    org.junit.jupiter.api.Assertions.assertThrows(
        RuntimeException.class,
        () -> rawProductListener.onHighVolumeRawProductUpdateCombinedEvent(testRecord)
    );

    verify(listenerService).saveProductAndRelated(eq(testRecord), eq(TEST_TRACE_ID));
  }
}
