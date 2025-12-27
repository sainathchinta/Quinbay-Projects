package com.gdn.pbp.config;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.MockitoAnnotations;

import com.blibli.oss.backend.kafka.model.ProducerEvent;
import com.gdn.mta.domain.event.modal.ImageQcProcessedResponseDomainEvent;

public class TimestampInterceptorTest {

  @InjectMocks
  private TimestampInterceptor timestampInterceptor;

  ProducerEvent event;
  ImageQcProcessedResponseDomainEvent imageQcProcessedResponseDomainEvent;

  @BeforeEach
  public void setup() {
    MockitoAnnotations.initMocks(this);
  }

  @Test
  public void beforeSendTest() {
    imageQcProcessedResponseDomainEvent = new ImageQcProcessedResponseDomainEvent();
    event = ProducerEvent.builder().value(imageQcProcessedResponseDomainEvent).build();
    timestampInterceptor.beforeSend(event);
    Assertions.assertNotNull(event.getValue());
    Assertions.assertNotNull(imageQcProcessedResponseDomainEvent.getTimestamp());
  }

  @Test
  public void beforeSendWhenNotAnInstanceTest() {
    event = ProducerEvent.builder().value(new String()).build();
    timestampInterceptor.beforeSend(event);
    Assertions.assertNotNull(event.getValue());
    Assertions.assertNull(event.getTimestamp());
  }
}
