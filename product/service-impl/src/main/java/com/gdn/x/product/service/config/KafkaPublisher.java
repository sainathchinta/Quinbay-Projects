package com.gdn.x.product.service.config;

import com.blibli.oss.backend.kafka.producer.KafkaProducer;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
@Slf4j
public class KafkaPublisher {

    @Autowired
    private KafkaProducer kafkaProducer;

    public void send(String topic, String key, Object payload) {
        kafkaProducer.send(topic, key, payload)
                .subscribe(sendResult -> log.info("Message successfully send : {} ", sendResult));
    }

    public void send(String topic, Object payload) {
        send(topic, null, payload);
    }
}
