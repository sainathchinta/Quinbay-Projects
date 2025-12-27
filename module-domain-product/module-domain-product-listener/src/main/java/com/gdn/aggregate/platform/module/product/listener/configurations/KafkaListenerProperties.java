package com.gdn.aggregate.platform.module.product.listener.configurations;

import lombok.Getter;
import lombok.Setter;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.context.annotation.Configuration;

@Configuration
@ConfigurationProperties(prefix = "kafka.listener")
@Getter
@Setter
public class KafkaListenerProperties {
    private boolean autoStartup;
}