package com.gdn.x.product.rest.web.interceptor;

import java.util.ArrayList;
import java.util.Objects;
import java.util.Optional;
import java.util.UUID;

import org.aspectj.lang.ProceedingJoinPoint;
import org.aspectj.lang.annotation.Around;
import org.aspectj.lang.annotation.Aspect;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.gdn.x.product.rest.web.properties.PrimaryDataSourceApis;
import com.gdn.x.product.service.constants.MongoReadPreference;
import com.gdn.x.product.service.interceptor.MandatoryParameterHelper;

import brave.Span;
import brave.Tracer;
import lombok.extern.slf4j.Slf4j;

@Component
@Aspect
@Slf4j
public class KafkaConsumerInterceptor {

  @Autowired
  private Tracer tracer;

  @Autowired
  private MandatoryParameterHelper mandatoryParameterHelper;

  @Autowired
  private PrimaryDataSourceApis primaryDataSourceApis;

  @Around(value = "@annotation(org.springframework.kafka.annotation.KafkaListener)")
  public Object beforeConsume(ProceedingJoinPoint joinPoint) throws Throwable {
    Object returnedValue;
    try {
      setReadPreference(joinPoint);
      returnedValue = joinPoint.proceed();
    } catch (Throwable e) {
      throw e;
    }
    return returnedValue;
  }

  private void setReadPreference(ProceedingJoinPoint joinPoint) {
    if (Optional.ofNullable(primaryDataSourceApis.getEvents()).orElse(new ArrayList<>())
        .contains(joinPoint.getTarget().getClass().getName())) {
      Span span = tracer.currentSpan();
      if (Objects.isNull(span)) {
        tracer.startScopedSpan(UUID.randomUUID().toString());
      }
      mandatoryParameterHelper.setReadPreference(MongoReadPreference.PRIMARY_PREFERRED.name());
    }
  }
 }
