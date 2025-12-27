package com.gdn.aggregate.platform.module.product.listener.listener.direct.sivaBoth;

import com.gdn.aggregate.platform.module.product.listener.service.helper.ListenerService;
import com.gdn.aggregate.platform.module.product.listener.configurations.TraceHelper;
import io.micrometer.tracing.Tracer;
import org.apache.kafka.clients.consumer.ConsumerRecord;
import org.apache.kafka.common.header.internals.RecordHeaders;
import org.apache.kafka.common.record.TimestampType;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.mockito.MockedStatic;

import static org.mockito.ArgumentMatchers.anyBoolean;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.*;

class DirectUpdateSivaBothByItemTest {

    // Constants for test data
    private static final String TEST_TOPIC = "test-item-topic";
    private static final int TEST_PARTITION = 0;
    private static final long TEST_OFFSET = 0L;
    private static final long TEST_TIMESTAMP = System.currentTimeMillis();
    private static final TimestampType TEST_TIMESTAMP_TYPE = TimestampType.CREATE_TIME;
    private static final long TEST_CHECKSUM = ConsumerRecord.NULL_CHECKSUM;
    private static final int TEST_SERIALIZED_KEY_SIZE = 3;
    private static final int TEST_SERIALIZED_VALUE_SIZE = 5;
    private static final String TEST_KEY = "key";
    private static final String TEST_VALUE = "value";
    private static final String MOCK_TRACE_ID_1 = "mock-trace-id-1";
    private static final String MOCK_TRACE_ID_2 = "mock-trace-id-2";

    @Mock
    private ListenerService listenerService;

    @Mock
    private Tracer tracer;

    @InjectMocks
    private DirectUpdateSivaBothByItem directUpdateSivaBothByItem;

    private AutoCloseable closeable;
    // Variable to hold the static mock context
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
    void testOnItemEventSivaProduct() throws Exception {
        ConsumerRecord<String, String> testRecord = createTestConsumerRecord();
        mockedTraceHelper.when(() -> TraceHelper.getTraceId(tracer)).thenReturn(MOCK_TRACE_ID_1);
        directUpdateSivaBothByItem.onItemEventSivaProduct(testRecord);
        verify(listenerService).onItemEventV2(eq(testRecord), anyString(), eq(false), eq(MOCK_TRACE_ID_1));
        mockedTraceHelper.verify(() -> TraceHelper.getTraceId(tracer));
    }

    @Test
    void testOnAllItemEventSivaProduct() throws Exception {
        ConsumerRecord<String, String> testRecord = createTestConsumerRecord();
        mockedTraceHelper.when(() -> TraceHelper.getTraceId(tracer)).thenReturn(MOCK_TRACE_ID_2);
        directUpdateSivaBothByItem.onAllItemEventSivaProduct(testRecord);
        verify(listenerService).onItemEventV2(eq(testRecord), anyString(), eq(true), eq(MOCK_TRACE_ID_2));
        mockedTraceHelper.verify(() -> TraceHelper.getTraceId(tracer));
    }
}
