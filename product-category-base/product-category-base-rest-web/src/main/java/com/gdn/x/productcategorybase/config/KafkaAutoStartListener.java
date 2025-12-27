package com.gdn.x.productcategorybase.config;

import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.boot.context.event.ApplicationStartedEvent;
import org.springframework.context.Lifecycle;
import org.springframework.context.event.EventListener;
import org.springframework.kafka.config.KafkaListenerEndpointRegistry;
import org.springframework.stereotype.Component;

@Component
@ConditionalOnProperty(value = "configuration.kafka.topic.autoStartup", havingValue = "false")
public class KafkaAutoStartListener {
  private KafkaListenerEndpointRegistry kafkaListenerEndpointRegistry;

  public KafkaAutoStartListener(KafkaListenerEndpointRegistry kafkaListenerEndpointRegistry) {
    this.kafkaListenerEndpointRegistry = kafkaListenerEndpointRegistry;
  }

  @EventListener(ApplicationStartedEvent.class)
  public void run(ApplicationStartedEvent event) {
    kafkaListenerEndpointRegistry.getListenerContainers().forEach(Lifecycle::start);
  }
}
