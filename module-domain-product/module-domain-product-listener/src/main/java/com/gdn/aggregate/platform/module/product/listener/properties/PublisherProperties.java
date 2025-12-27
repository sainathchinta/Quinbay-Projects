package com.gdn.aggregate.platform.module.product.listener.properties;

import lombok.Getter;
import lombok.Setter;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.stereotype.Component;

@Setter
@Getter
@Component
@ConfigurationProperties(prefix = "publisher.throttle")
public class PublisherProperties {
    private int concurrency;
    public boolean isThrottlingEnabled() {
        return concurrency > 0;
    }
}
