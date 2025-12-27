package com.gdn.aggregate.platform.module.product.listener.listener.direct.sivaBoth;

import com.gdn.aggregate.platform.module.product.listener.service.helper.ListenerService;
import io.micrometer.tracing.Tracer;
import io.micrometer.tracing.Span;
import io.micrometer.tracing.TraceContext;
import org.apache.kafka.clients.consumer.ConsumerRecord;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.util.Arrays;
import java.util.Collections;

import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
class ProductElasticSearchDeletionListenerTest {

    @Mock
    private ListenerService listenerService;

    @Mock
    private Tracer tracer;

    @Mock
    private Span span;

    @Mock
    private TraceContext traceContext;

    @InjectMocks
    private ProductElasticSearchDeletionListener listener;

    private static final String TRACE_ID = "test-trace-id";
    private static final String RECORD_KEY = "test-key";
    private static final String RECORD_VALUE = "test-value";

    @BeforeEach
    void setUp() {
        when(tracer.currentSpan()).thenReturn(span);
        when(span.context()).thenReturn(traceContext);
        when(traceContext.traceId()).thenReturn(TRACE_ID);
    }

    @Test
    void onPermanentDeleteDataEvent_Success() {
        // Arrange
        ConsumerRecord<String, String> record = new ConsumerRecord<>("topic", 0, 0L, RECORD_KEY, RECORD_VALUE);

        // Act
        listener.onPermanentDeleteDataEvent(Arrays.asList(record));

        // Assert
        verify(listenerService).onElasticSearchDeletionEvent(eq(record), eq(TRACE_ID));
        verifyNoMoreInteractions(listenerService);
    }

    @Test
    void onPermanentDeleteDataEvent_MultipleRecords_Success() {
        // Arrange
        ConsumerRecord<String, String> record1 = new ConsumerRecord<>("topic", 0, 0L, RECORD_KEY + "1", RECORD_VALUE);
        ConsumerRecord<String, String> record2 = new ConsumerRecord<>("topic", 0, 1L, RECORD_KEY + "2", RECORD_VALUE);

        // Act
        listener.onPermanentDeleteDataEvent(Arrays.asList(record1, record2));

        // Assert
        verify(listenerService).onElasticSearchDeletionEvent(eq(record1), eq(TRACE_ID));
        verify(listenerService).onElasticSearchDeletionEvent(eq(record2), eq(TRACE_ID));
        verifyNoMoreInteractions(listenerService);
    }

    @Test
    void onPermanentDeleteDataEvent_EmptyRecords() {
        // Act
        listener.onPermanentDeleteDataEvent(Collections.emptyList());

        // Assert
        verifyNoInteractions(listenerService);
    }

    @Test
    void onPermanentDeleteDataEvent_ErrorHandling() {
        // Arrange
        ConsumerRecord<String, String> record1 = new ConsumerRecord<>("topic", 0, 0L, RECORD_KEY + "1", RECORD_VALUE);
        ConsumerRecord<String, String> record2 = new ConsumerRecord<>("topic", 0, 1L, RECORD_KEY + "2", RECORD_VALUE);
        
        doThrow(new RuntimeException("Test error")).when(listenerService)
            .onElasticSearchDeletionEvent(eq(record1), eq(TRACE_ID));

        // Act
        listener.onPermanentDeleteDataEvent(Arrays.asList(record1, record2));

        // Assert
        verify(listenerService).onElasticSearchDeletionEvent(eq(record1), eq(TRACE_ID));
        verify(listenerService).onElasticSearchDeletionEvent(eq(record2), eq(TRACE_ID));
        verifyNoMoreInteractions(listenerService);
    }
}