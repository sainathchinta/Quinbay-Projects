package com.gdn.partners.pcu.external.service.impl.config;

import com.blibli.oss.backend.kafka.producer.KafkaProducer;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.kafka.support.SendResult;
import reactor.core.publisher.Mono;

import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
class KafkaPublisherTest {

  @InjectMocks
  KafkaPublisher kafkaPublisher;

  @Mock
  KafkaProducer kafkaProducer;

  @Mock
  SendResult<String, String> sendResult;

  private static final String TOPIC = "topic";
  private static final String KEY = "key";
  private static final Object message = new Object();


  @Test
  void send_withKey_shouldCallKafkaProducerSend() {
    when(kafkaProducer.send(TOPIC, KEY, message)).thenReturn(Mono.just(sendResult));

    kafkaPublisher.send(TOPIC, KEY, message);

    verify(kafkaProducer, times(1)).send(TOPIC, KEY, message);
    verifyNoMoreInteractions(kafkaProducer);
  }

  @Test
  void send_withoutKey_shouldDelegateToSendWithNullKey() {
    when(kafkaProducer.send(TOPIC, null, message)).thenReturn(Mono.just(sendResult));

    kafkaPublisher.send(TOPIC, message);

    verify(kafkaProducer, times(1)).send(TOPIC, null, message);
    verifyNoMoreInteractions(kafkaProducer);
  }
}