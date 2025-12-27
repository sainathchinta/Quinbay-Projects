package com.gdn.partners.product.analytics.service.impl.helper;

import com.blibli.oss.backend.kafka.producer.KafkaProducer;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.kafka.support.SendResult;
import reactor.core.publisher.Mono;

import static org.mockito.Mockito.mock;

public class KafkaPublisherTest {

  @InjectMocks
  private KafkaPublisher kafkaPublisher;

  @Mock
  private KafkaProducer kafkaProducer;

  public static final String TOPIC = "TOPIC";
  public static final String KEY = "KEY";

  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.openMocks(this);
  }

  @Test
  void success_test() {
    SendResult<String, String> sendResult = mock(SendResult.class);
    Mono<SendResult<String, String>> singleSendResult = Mono.just(sendResult);
    Mockito.when(kafkaProducer.send(Mockito.any(), Mockito.any(), Mockito.any())).thenReturn(singleSendResult);
    kafkaPublisher.send(TOPIC,KEY, new Object());
    Mockito.verify(kafkaProducer).send(Mockito.any(), Mockito.any(), Mockito.any());
  }
}
