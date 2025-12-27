package com.gdn.aggregate.platform.module.product.listener.configurations;

import com.gdn.aggregate.modules.agp.engagement.common.util.helper.DummyHelper;
import io.micrometer.tracing.Span;
import io.micrometer.tracing.TraceContext;
import io.micrometer.tracing.Tracer;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.Mock;
import org.mockito.Mockito;

import java.util.UUID;

import static org.junit.jupiter.api.Assertions.fail;

public class TraceHelperTest extends DummyHelper {

  @Mock
  private Tracer tracer;

  @Mock
  private Span span;

  @Mock
  private TraceContext traceContext;

  private final String testTraceId = "test-trace-id-123";
  private final RuntimeException testException = new RuntimeException("Test Error");

  @BeforeEach
  void setUp() {
    Mockito.lenient().when(tracer.currentSpan()).thenReturn(span); // Use lenient stubbing
    Mockito.lenient().when(span.context()).thenReturn(traceContext);
    Mockito.lenient().when(traceContext.traceId()).thenReturn(testTraceId);
  }

  @Test
  void getTraceId_shouldReturnTraceId_whenContextExists() {
    String traceId = TraceHelper.getTraceId(tracer);
    Assertions.assertEquals(testTraceId, traceId);
    Mockito.verify(tracer).currentSpan();
    Mockito.verify(span).context();
    Mockito.verify(traceContext).traceId();
  }

  @Test
  void getTraceId_shouldReturnUUID_whenTracerIsNull() {
    String traceId = TraceHelper.getTraceId(null);
    Assertions.assertNotNull(traceId);
    try {
      UUID.fromString(traceId);
    } catch (IllegalArgumentException e) {
      fail("Expected a valid UUID string when tracer is null");
    }
  }

  @Test
  void getTraceId_shouldReturnUUID_whenCurrentSpanIsNull() {
    Mockito.when(tracer.currentSpan()).thenReturn(null);
    String traceId = TraceHelper.getTraceId(tracer);
    Assertions.assertNotNull(traceId);
    try {
      UUID.fromString(traceId);
    } catch (IllegalArgumentException e) {
      fail("Expected a valid UUID string when current span is null");
    }

    Mockito.verify(tracer).currentSpan();
    Mockito.verify(span, Mockito.never()).context();
  }

  @Test
  void getTraceId_shouldReturnUUID_whenContextIsNull() {
    Mockito.when(span.context()).thenReturn(null);
    String traceId = TraceHelper.getTraceId(tracer);
    Assertions.assertNotNull(traceId);
    try {
      UUID.fromString(traceId);
    } catch (IllegalArgumentException e) {
      fail("Expected a valid UUID string when context is null");
    }
    Mockito.verify(tracer).currentSpan();
    Mockito.verify(span).context();
    Mockito.verify(traceContext, Mockito.never()).traceId();
  }

  @Test
  void recordError_shouldCallSpanError_whenTracerAndSpanAndThrowableExist() {
    TraceHelper.recordError(tracer, testException);
    Mockito.verify(tracer).currentSpan();
    Mockito.verify(span).error(testException);
  }

  @Test
  void recordError_shouldDoNothing_whenCurrentSpanIsNull() {
    Mockito.when(tracer.currentSpan()).thenReturn(null);
    Assertions.assertDoesNotThrow(() -> TraceHelper.recordError(tracer, testException));
    Mockito.verify(tracer).currentSpan();
    Mockito.verify(span, Mockito.never()).error(Mockito.any(Throwable.class));
  }

  @Test
  void recordError_shouldDoNothing_whenThrowableIsNull() {
    Assertions.assertDoesNotThrow(() -> TraceHelper.recordError(tracer, null));
    Mockito.verify(tracer).currentSpan();
    Mockito.verify(span, Mockito.never()).error(Mockito.any(Throwable.class));
  }
}