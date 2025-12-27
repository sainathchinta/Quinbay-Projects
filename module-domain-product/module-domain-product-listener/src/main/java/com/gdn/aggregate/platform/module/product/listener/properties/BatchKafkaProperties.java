package com.gdn.aggregate.platform.module.product.listener.properties;

import lombok.Data;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.context.annotation.Configuration;

@Data
@Configuration
@ConfigurationProperties(prefix = "module.domain.product.batch.kafka")
public class BatchKafkaProperties {
    private int fetchMaxWaitMs;
    private int fetchMinBytes;
    private int maxPollRecords;
    private boolean debugEnabled;
    
    private int permanentDeleteMaxPollRecords;
    private int permanentDeleteFetchMaxWaitMs;
    private int permanentDeleteFetchMinBytes;
    private int permanentDeleteMaxPollIntervalMs;
    private int permanentDeleteFetchMaxBytes;
} 