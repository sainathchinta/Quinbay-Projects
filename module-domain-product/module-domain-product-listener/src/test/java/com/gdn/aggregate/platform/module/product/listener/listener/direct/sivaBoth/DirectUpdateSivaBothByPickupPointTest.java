package com.gdn.aggregate.platform.module.product.listener.listener.direct.sivaBoth;

import com.gdn.aggregate.platform.module.product.listener.configurations.TraceHelper;
import com.gdn.aggregate.platform.module.product.listener.constants.Topics; // Import Topics
import com.gdn.aggregate.platform.module.product.listener.service.helper.ListenerService;
import io.micrometer.tracing.Tracer;
import org.apache.kafka.clients.consumer.ConsumerRecord;
import org.apache.kafka.common.header.internals.RecordHeaders;
import org.apache.kafka.common.record.TimestampType;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockedStatic;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.verify;

class DirectUpdateSivaBothByPickupPointTest {

  // Constants for test data
  // Note: We use different topics for the two methods, but the same group ID
  private static final String PICKUP_POINT_TOPIC = Topics.PICKUP_POINT;
  private static final String ALL_PICKUP_POINT_TOPIC = Topics.ALL_PICKUP_POINT;
  private static final String TEST_GROUP_ID = Topics.GROUP_ID_SIVA_BOTH_BY_PICKUP_POINT;
  private static final int TEST_PARTITION = 0;
  private static final long TEST_OFFSET = 0L;
  private static final long TEST_TIMESTAMP = System.currentTimeMillis();
  private static final TimestampType TEST_TIMESTAMP_TYPE = TimestampType.CREATE_TIME;
  private static final long TEST_CHECKSUM = ConsumerRecord.NULL_CHECKSUM;
  private static final int TEST_SERIALIZED_KEY_SIZE = 4;
  private static final int TEST_SERIALIZED_VALUE_SIZE = 6;
  private static final String TEST_KEY = "pp-key";
  private static final String TEST_VALUE = "pp-value";
  private static final String MOCK_TRACE_ID_1 = "mock-trace-id-pp-1";
  private static final String MOCK_TRACE_ID_2 = "mock-trace-id-pp-2";

  @Mock
  private ListenerService listenerService;

  @Mock
  private Tracer tracer;

  @InjectMocks
  private DirectUpdateSivaBothByPickupPoint pickupPointListener; // Renamed instance

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

  private ConsumerRecord<String, String> createTestConsumerRecord(String topic) {
    return new ConsumerRecord<>(
      topic, TEST_PARTITION, TEST_OFFSET, TEST_TIMESTAMP,
      TEST_TIMESTAMP_TYPE, TEST_CHECKSUM, TEST_SERIALIZED_KEY_SIZE, TEST_SERIALIZED_VALUE_SIZE,
      TEST_KEY, TEST_VALUE, new RecordHeaders(), null
    );
  }

  @Test
  void testOnPickupPointEventSivaBoth() throws Exception {
    ConsumerRecord<String, String> testRecord = createTestConsumerRecord(PICKUP_POINT_TOPIC);
    mockedTraceHelper.when(() -> TraceHelper.getTraceId(tracer)).thenReturn(MOCK_TRACE_ID_1);
    pickupPointListener.onPickupPointEventSivaBoth(testRecord);
    verify(listenerService).onPickupPointEventV2(
      eq(testRecord),
      eq(TEST_GROUP_ID),
      eq(false),
      eq(false),
      eq(MOCK_TRACE_ID_1)
    );
    mockedTraceHelper.verify(() -> TraceHelper.getTraceId(tracer));
  }

  @Test
  void testOnAllPickupPointEventSivaBoth() throws Exception {
    ConsumerRecord<String, String> testRecord = createTestConsumerRecord(ALL_PICKUP_POINT_TOPIC);
    mockedTraceHelper.when(() -> TraceHelper.getTraceId(tracer)).thenReturn(MOCK_TRACE_ID_2);
    pickupPointListener.onAllPickupPointEventSivaBoth(testRecord);
    verify(listenerService).onPickupPointEventV2(
      eq(testRecord),
      eq(TEST_GROUP_ID),
      eq(true),
      eq(true),
      eq(MOCK_TRACE_ID_2)
    );
    mockedTraceHelper.verify(() -> TraceHelper.getTraceId(tracer));
  }
}