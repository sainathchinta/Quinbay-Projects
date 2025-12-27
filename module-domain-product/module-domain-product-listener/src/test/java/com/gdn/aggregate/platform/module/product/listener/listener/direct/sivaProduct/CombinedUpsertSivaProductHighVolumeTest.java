package com.gdn.aggregate.platform.module.product.listener.listener.direct.sivaProduct;

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
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.verify;

class CombinedUpsertSivaProductHighVolumeTest {

  private static final String TEST_TOPIC = Topics.SIVA_PRODUCT_COMBINED_UPSERT_HIGH_VOLUME;
  private static final int TEST_PARTITION = 0;
  private static final long TEST_OFFSET = 0L;
  private static final String TEST_KEY = "siva-product-key";
  private static final String TEST_VALUE = "siva-product-value";
  private static final String MOCK_TRACE_ID = "mock-trace-id-siva-product";

  @Mock
  private ListenerService listenerService;

  @Mock
  private Tracer tracer;

  @InjectMocks
  private CombinedUpsertSivaProductHighVolume sivaProductListener;

  private AutoCloseable closeable;
  private MockedStatic<TraceHelper> mockedTraceHelper;

  @BeforeEach
  void setUp() {
    closeable = MockitoAnnotations.openMocks(this);
    mockedTraceHelper = Mockito.mockStatic(TraceHelper.class);
  }

  @AfterEach
  void tearDown() throws Exception {
    mockedTraceHelper.close();
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
  void testOnSivaProductCombinedUpsertEvent() throws Exception {
    ConsumerRecord<String, String> testRecord = createTestConsumerRecord();
    mockedTraceHelper.when(() -> TraceHelper.getTraceId(tracer)).thenReturn(MOCK_TRACE_ID);
    
    sivaProductListener.onSivaProductCombinedUpsertEvent(testRecord);
    
    verify(listenerService).onSivaProductCombinedUpsertEvent(eq(testRecord), eq(MOCK_TRACE_ID));
    mockedTraceHelper.verify(() -> TraceHelper.getTraceId(tracer));
  }

  @Test
  void testOnSivaProductCombinedUpsertEvent_WhenExceptionThrown() throws Exception {
    ConsumerRecord<String, String> testRecord = createTestConsumerRecord();
    mockedTraceHelper.when(() -> TraceHelper.getTraceId(tracer)).thenReturn(MOCK_TRACE_ID);
    RuntimeException testException = new RuntimeException();
    doThrow(testException)
        .when(listenerService)
        .onSivaProductCombinedUpsertEvent(testRecord, MOCK_TRACE_ID);

    org.junit.jupiter.api.Assertions.assertThrows(
      RuntimeException.class,
        () -> sivaProductListener.onSivaProductCombinedUpsertEvent(testRecord)
    );

    verify(listenerService).onSivaProductCombinedUpsertEvent(eq(testRecord), eq(MOCK_TRACE_ID));
    mockedTraceHelper.verify(() -> TraceHelper.getTraceId(tracer));
  }

  @Test
  void testOnSivaProductCombinedUpsertEvent_WhenRuntimeExceptionThrown() throws Exception {
    ConsumerRecord<String, String> testRecord = createTestConsumerRecord();
    mockedTraceHelper.when(() -> TraceHelper.getTraceId(tracer)).thenReturn(MOCK_TRACE_ID);
    RuntimeException testException = new RuntimeException("Test runtime exception");
    doThrow(testException)
        .when(listenerService)
        .onSivaProductCombinedUpsertEvent(testRecord, MOCK_TRACE_ID);

    org.junit.jupiter.api.Assertions.assertThrows(
        RuntimeException.class,
        () -> sivaProductListener.onSivaProductCombinedUpsertEvent(testRecord)
    );

    verify(listenerService).onSivaProductCombinedUpsertEvent(eq(testRecord), eq(MOCK_TRACE_ID));
    mockedTraceHelper.verify(() -> TraceHelper.getTraceId(tracer));
  }
}
