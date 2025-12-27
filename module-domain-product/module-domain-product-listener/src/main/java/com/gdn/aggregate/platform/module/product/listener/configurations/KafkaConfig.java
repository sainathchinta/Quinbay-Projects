package com.gdn.aggregate.platform.module.product.listener.configurations;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.boot.autoconfigure.kafka.KafkaProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.kafka.config.ConcurrentKafkaListenerContainerFactory;
import org.springframework.kafka.core.ConsumerFactory;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.kafka.config.KafkaListenerEndpointRegistry;
import org.springframework.context.event.EventListener;
import org.springframework.boot.context.event.ApplicationStartedEvent;
import org.springframework.context.Lifecycle;

@Configuration
@ConditionalOnProperty(name = "custom.kafka.configs.enabled", havingValue = "true")
public class KafkaConfig {

    private final KafkaListenerProperties listenerProperties;
    private final KafkaListenerEndpointRegistry kafkaListenerEndpointRegistry;

    @Value("${spring.threads.virtual.enabled}")
    private boolean virtualThreadsEnabled;

    @Autowired
    private KafkaProperties kafkaProperties;

    public KafkaConfig(KafkaListenerProperties listenerProperties,
      KafkaListenerEndpointRegistry kafkaListenerEndpointRegistry) {
        this.listenerProperties = listenerProperties;
        this.kafkaListenerEndpointRegistry = kafkaListenerEndpointRegistry;
    }

    /**
     * AutoStartup is controlled by the 'kafka.listener.autoStartup' property.
     * @param consumerFactory Spring Boot default consumer factory.
     * @return Configured ConcurrentKafkaListenerContainerFactory.
     */
    @Bean
    public ConcurrentKafkaListenerContainerFactory<Object, Object> kafkaListenerContainerFactory(
      ConsumerFactory<Object, Object> consumerFactory) {
        ConcurrentKafkaListenerContainerFactory<Object, Object> factory =
          new ConcurrentKafkaListenerContainerFactory<>();
        factory.setConsumerFactory(consumerFactory);
        if (virtualThreadsEnabled) {
            factory.getContainerProperties().setListenerTaskExecutor(null);
        }
        factory.setAutoStartup(listenerProperties.isAutoStartup());
        return factory;
    }

    @EventListener(ApplicationStartedEvent.class)
    public void startKafkaListeners(ApplicationStartedEvent event) {
        // Start listeners manually only if autoStartup is false
        if (!listenerProperties.isAutoStartup()) {
            kafkaListenerEndpointRegistry.getListenerContainers().forEach(Lifecycle::start);
        }
    }
} 