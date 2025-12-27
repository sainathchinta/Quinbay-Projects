package com.gdn.aggregate.platform.module.product.listener.configurations;

import io.micrometer.tracing.Span;
import io.micrometer.tracing.Tracer;
import io.micrometer.tracing.TraceContext;
import org.springframework.stereotype.Component;

import java.util.Objects;
import java.util.Optional;
import java.util.UUID;

@Component
public class TraceHelper {

    public static String getTraceId(Tracer tracer) {
        return Optional.ofNullable(tracer).map(Tracer::currentSpan).map(Span::context)
          .map(TraceContext::traceId).orElseGet(() -> UUID.randomUUID().toString());
    }
    
    public static void recordError(Tracer tracer, Throwable throwable) {
        Span currentSpan = tracer.currentSpan();
        if (Objects.nonNull(currentSpan) && Objects.nonNull(throwable)) {
            currentSpan.error(throwable);
        }
    }
}