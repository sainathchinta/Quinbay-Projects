package com.gdn.mta.product.config;

import org.aspectj.lang.ProceedingJoinPoint;
import org.aspectj.lang.annotation.Around;
import org.aspectj.lang.annotation.Aspect;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import brave.Tracer;
import brave.propagation.CurrentTraceContext;
import brave.propagation.TraceContext;
import lombok.extern.slf4j.Slf4j;

@Slf4j
@Aspect
@Component
public class KafkaTracingAspect {

    @Autowired
    private Tracer tracer;

    @Autowired
    private CurrentTraceContext currentTraceContext;

    @Around("@annotation(org.springframework.kafka.annotation.KafkaListener)")
    public Object aroundKafkaListener(ProceedingJoinPoint joinPoint) throws Throwable {
        // Initialize tracing context if not present
        if (tracer.currentSpan() == null) {
            TraceContext context = tracer.newTrace().context();
            try (CurrentTraceContext.Scope scope = currentTraceContext.newScope(context)) {
                return joinPoint.proceed();
            }
        } else {
            return joinPoint.proceed();
        }
    }
} 