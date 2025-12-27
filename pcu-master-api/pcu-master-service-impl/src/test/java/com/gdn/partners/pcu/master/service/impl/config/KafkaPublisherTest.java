package com.gdn.partners.pcu.master.service.impl.config;

import com.blibli.oss.backend.kafka.producer.KafkaProducer;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;

import org.springframework.kafka.support.SendResult;
import reactor.core.publisher.Mono;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
public class KafkaPublisherTest {

  @InjectMocks
  private KafkaPublisher kafkaPublisher;
  @Mock
  private KafkaProducer kafkaProducer;

  private static final String TOPIC_NAME = "topic.name";
  private static final String PARTITION_KEY = "partitionKey";
  private static final String PAYLOAD = "payload";

  @AfterEach
  void teardown() {
    Mockito.verifyNoMoreInteractions(kafkaProducer);
  }

  @Test
  void sendEvent_test() {
    SendResult<String, String> sendResult = mock(SendResult.class);
    Mono<SendResult<String, String>> singleSendResult = Mono.just(sendResult);
    when(kafkaProducer.send(TOPIC_NAME, PARTITION_KEY, PAYLOAD)).thenReturn(singleSendResult);
    kafkaPublisher.send(TOPIC_NAME, PARTITION_KEY, PAYLOAD);
    verify(kafkaProducer).send(TOPIC_NAME, PARTITION_KEY, PAYLOAD);
  }

}
