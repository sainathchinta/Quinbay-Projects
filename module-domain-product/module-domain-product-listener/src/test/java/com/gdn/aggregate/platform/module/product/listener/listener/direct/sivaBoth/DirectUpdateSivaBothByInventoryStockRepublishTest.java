package com.gdn.aggregate.platform.module.product.listener.listener.direct.sivaBoth;

import com.gdn.aggregate.platform.module.product.listener.configurations.TraceHelper;
import com.gdn.aggregate.platform.module.product.listener.constants.Topics;
import com.gdn.aggregate.platform.module.product.listener.service.helper.ListenerService;
import io.micrometer.tracing.Tracer;
import org.apache.kafka.clients.consumer.ConsumerRecord;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockedStatic;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;

import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
class DirectUpdateSivaBothByInventoryStockRepublishTest {

    @Mock
    private ListenerService listenerService;

    @Mock
    private Tracer tracer;

    @InjectMocks
    private DirectUpdateSivaBothByInventoryStockRepublish listener;

    private static final String TRACE_ID = "test-trace-id";
    private static final String RECORD_VALUE = "{\"test\":\"value\"}";
    private static final String RECORD_KEY = "test-key";
    private static final String TOPIC = Topics.INVENTORY_ONLINE_REPUBLISH_EVENT;

    @Test
    void onInventoryInfoChangeEventSivaBoth_shouldCallListenerService() throws Exception {
        ConsumerRecord<String, String> record =
          new ConsumerRecord<>(TOPIC, 0, 0L, RECORD_KEY, RECORD_VALUE);
        try (MockedStatic<TraceHelper> traceHelper = Mockito.mockStatic(TraceHelper.class)) {
            traceHelper.when(() -> TraceHelper.getTraceId(tracer)).thenReturn(TRACE_ID);
            listener.onInventoryInfoChangeEventSivaBoth(record);
            verify(listenerService, times(1)).publishPickupPointUpsertEventOnInventoryRepublish(
              record, TRACE_ID);
        }
    }

    @Test
    void onInventoryInfoChangeEventSivaBoth_whenTraceIdIsNull_shouldStillProcessRecord() throws Exception {
        ConsumerRecord<String, String> record = new ConsumerRecord<>(TOPIC, 0, 0L, RECORD_KEY, RECORD_VALUE);
        try (MockedStatic<TraceHelper> traceHelper = Mockito.mockStatic(TraceHelper.class)) {
            traceHelper.when(() -> TraceHelper.getTraceId(tracer)).thenReturn(null);
            listener.onInventoryInfoChangeEventSivaBoth(record);
            verify(listenerService, times(1))
                .publishPickupPointUpsertEventOnInventoryRepublish(record, null);
        }
    }

    @Test
    void onInventoryInfoChangeEventSivaBoth_whenRecordHasNullValue_shouldStillProcess() throws Exception {
        ConsumerRecord<String, String> record = new ConsumerRecord<>(TOPIC, 0, 0L, RECORD_KEY, null);
        try (MockedStatic<TraceHelper> traceHelper = Mockito.mockStatic(TraceHelper.class)) {
            traceHelper.when(() -> TraceHelper.getTraceId(tracer)).thenReturn(TRACE_ID);
            listener.onInventoryInfoChangeEventSivaBoth(record);
            verify(listenerService, times(1))
                .publishPickupPointUpsertEventOnInventoryRepublish(record, TRACE_ID);
        }
    }
}
