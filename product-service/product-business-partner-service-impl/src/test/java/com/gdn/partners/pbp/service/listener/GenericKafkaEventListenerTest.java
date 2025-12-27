package com.gdn.partners.pbp.service.listener;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import com.gdn.mta.product.service.config.KafkaTopicProperties;
import com.gdn.partners.pbp.commons.constants.Constants;

public class GenericKafkaEventListenerTest {

  @InjectMocks
  private GenericKafkaEventListener genericKafkaEventListener;

  @Mock
  private KafkaTopicProperties kafkaTopicProperties;

  @BeforeEach
  public void setUp() {
    MockitoAnnotations.initMocks(this);
  }

  @AfterEach
  public void tearDown() {
    Mockito.verifyNoMoreInteractions(kafkaTopicProperties);
  }

  @Test
  public void onDomainEventConsumed() throws Exception {
    genericKafkaEventListener.onDomainEventConsumed(Constants.ACTIVE);
    Mockito.verify(kafkaTopicProperties).getGenericKafkaEventName();
    Mockito.verify(kafkaTopicProperties).getGenericKafkaGroupId();
    Mockito.verify(kafkaTopicProperties).getGenericKafkaEventConcurrency();
  }
}