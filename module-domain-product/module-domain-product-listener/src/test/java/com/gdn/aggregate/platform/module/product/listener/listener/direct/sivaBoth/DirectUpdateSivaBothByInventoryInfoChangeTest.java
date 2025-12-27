package com.gdn.aggregate.platform.module.product.listener.listener.direct.sivaBoth;

import com.gdn.aggregate.platform.module.product.listener.configurations.TraceHelper;
import com.gdn.aggregate.platform.module.product.listener.constants.Topics; // Import Topics if needed for verification
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

class DirectUpdateSivaBothByInventoryInfoChangeTest {

  // Constants for test data
  private static final String TEST_TOPIC = Topics.INVENTORY_INFO_CHANGE; // Use actual topic
  private static final String TEST_GROUP_ID = Topics.GROUP_ID_SIVA_BOTH_BY_INVENTORY_INFO_CHANGE; // Use actual group ID
  private static final int TEST_PARTITION = 0;
  private static final long TEST_OFFSET = 0L;
  private static final long TEST_TIMESTAMP = System.currentTimeMillis();
  private static final TimestampType TEST_TIMESTAMP_TYPE = TimestampType.CREATE_TIME;
  private static final long TEST_CHECKSUM = ConsumerRecord.NULL_CHECKSUM;
  private static final int TEST_SERIALIZED_KEY_SIZE = 3;
  private static final int TEST_SERIALIZED_VALUE_SIZE = 5;
  private static final String TEST_KEY = "inv-key";
  private static final String TEST_VALUE = "inv-value";
  private static final String MOCK_TRACE_ID = "mock-trace-id-inv-change";

  @Mock
  private ListenerService listenerService;

  @Mock
  private Tracer tracer;

  @InjectMocks
  private DirectUpdateSivaBothByInventoryInfoChange inventoryInfoChangeListener; // Renamed instance

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
      TEST_TOPIC, TEST_PARTITION, TEST_OFFSET, TEST_TIMESTAMP,
      TEST_TIMESTAMP_TYPE, TEST_CHECKSUM, TEST_SERIALIZED_KEY_SIZE, TEST_SERIALIZED_VALUE_SIZE,
      TEST_KEY, TEST_VALUE, new RecordHeaders(), null
    );
  }

  @Test
  void testOnInventoryInfoChangeEventSivaBoth() throws Exception {
    ConsumerRecord<String, String> testRecord = createTestConsumerRecord();
    mockedTraceHelper.when(() -> TraceHelper.getTraceId(tracer)).thenReturn(MOCK_TRACE_ID);

    inventoryInfoChangeListener.onInventoryInfoChangeEventSivaBoth(testRecord);
    verify(listenerService).onInventoryInfoChangeEvent(
      eq(testRecord),
      eq(TEST_GROUP_ID), // Verify the correct group ID is passed
      eq(MOCK_TRACE_ID)
    );
    mockedTraceHelper.verify(() -> TraceHelper.getTraceId(tracer));
  }
}