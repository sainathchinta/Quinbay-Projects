package com.gdn.x.product.service.kafka;

import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.kafka.config.ContainerCustomizer;
import org.springframework.kafka.listener.ConcurrentMessageListenerContainer;
import org.springframework.stereotype.Component;

import lombok.extern.slf4j.Slf4j;


@Component
@Slf4j
@ConditionalOnProperty(name = "spring.threads.virtual.enabled", havingValue = "true")
public class KafkaContainerCustomizer
    implements ContainerCustomizer<Object, Object, ConcurrentMessageListenerContainer<Object, Object>> {

  @Override
  public void configure(ConcurrentMessageListenerContainer<Object, Object> container) {
    log.info("Customizing container: {}", container);
    container.getContainerProperties().setListenerTaskExecutor(null);
  }
}
