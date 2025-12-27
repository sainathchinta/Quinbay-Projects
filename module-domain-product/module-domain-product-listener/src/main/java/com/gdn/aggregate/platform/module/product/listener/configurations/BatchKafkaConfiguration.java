package com.gdn.aggregate.platform.module.product.listener.configurations;

import com.gdn.aggregate.platform.module.product.listener.constants.Enabler;
import com.gdn.aggregate.platform.module.product.listener.properties.BatchKafkaProperties;
import org.apache.kafka.clients.consumer.ConsumerConfig;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.boot.autoconfigure.kafka.KafkaProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.kafka.config.ConcurrentKafkaListenerContainerFactory;
import org.springframework.kafka.core.ConsumerFactory;
import org.springframework.kafka.core.DefaultKafkaConsumerFactory;

import java.util.HashMap;
import java.util.Map;

@Configuration
@ConditionalOnProperty(name = Enabler.FLOW_SIVA_ITEM_BATCH_DEDUPLICATION_ENABLED, havingValue = "true")
public class BatchKafkaConfiguration {

    @Autowired
    private KafkaProperties kafkaProperties;

    @Autowired
    private BatchKafkaProperties batchKafkaProperties;

    @Value("${spring.threads.virtual.enabled}")
    private boolean virtualThreadsEnabled;

    @Value("${spring.kafka.listener.concurrency}")
    private int kafkaConcurrency;

    // Properties are now managed in BatchKafkaProperties

    @Bean("batchDeduplicationConsumerFactory")
    public ConsumerFactory<Object, Object> batchDeduplicationConsumerFactory() {
        Map<String, Object> props = new HashMap<>(kafkaProperties.buildConsumerProperties());
        // the broker waits up to fetch.max.wait.ms for more messages to arrive before sending a response
        props.put(ConsumerConfig.FETCH_MAX_WAIT_MS_CONFIG, batchKafkaProperties.getFetchMaxWaitMs());
        // the minimum amount of data the broker should return for a fetch request
        // this helps to accumulate more records in a single batch
        props.put(ConsumerConfig.FETCH_MIN_BYTES_CONFIG, batchKafkaProperties.getFetchMinBytes());
        // the maximum number of records returned in a single poll
        props.put(ConsumerConfig.MAX_POLL_RECORDS_CONFIG, batchKafkaProperties.getMaxPollRecords());
        
        return new DefaultKafkaConsumerFactory<>(props);
    }

    @Bean("batchDeduplicationContainerFactory")
    public ConcurrentKafkaListenerContainerFactory<Object, Object> batchDeduplicationContainerFactory() {
        ConcurrentKafkaListenerContainerFactory<Object, Object> factory = 
            new ConcurrentKafkaListenerContainerFactory<>();
        factory.setConsumerFactory(batchDeduplicationConsumerFactory());
        factory.setBatchListener(true);
        if (virtualThreadsEnabled) {
            factory.getContainerProperties().setListenerTaskExecutor(null);
        }
        factory.setAutoStartup(true);
        return factory;
    }

    @Bean("permanentDeletionContainerFactory")
    public ConcurrentKafkaListenerContainerFactory<Object, Object> permanentDeletionContainerFactory() {
        ConcurrentKafkaListenerContainerFactory<Object, Object> factory =
            new ConcurrentKafkaListenerContainerFactory<>();
        Map<String, Object> props = new HashMap<>(kafkaProperties.buildConsumerProperties());
        // maximum number of records returned in a single poll
        props.put(ConsumerConfig.MAX_POLL_RECORDS_CONFIG, batchKafkaProperties.getPermanentDeleteMaxPollRecords());
        // max time broker will wait for new fetch until min byte
        props.put(ConsumerConfig.FETCH_MAX_WAIT_MS_CONFIG, batchKafkaProperties.getPermanentDeleteFetchMaxWaitMs());
        // broker waits until at least min of this data is available
        props.put(ConsumerConfig.FETCH_MIN_BYTES_CONFIG, batchKafkaProperties.getPermanentDeleteFetchMinBytes());
        // Increased max poll interval to handle long inactive periods (2 hours)
        // since our scheduler will run only during off business hours
        props.put(ConsumerConfig.MAX_POLL_INTERVAL_MS_CONFIG, batchKafkaProperties.getPermanentDeleteMaxPollIntervalMs());
        props.put(ConsumerConfig.SESSION_TIMEOUT_MS_CONFIG, 300000);
        // Maximum total bytes returned in one fetch request (across all partitions)
        props.put(ConsumerConfig.FETCH_MAX_BYTES_CONFIG, batchKafkaProperties.getPermanentDeleteFetchMaxBytes());
        // Maximum bytes returned per partition per fetch - 10MB
        props.put(ConsumerConfig.MAX_PARTITION_FETCH_BYTES_CONFIG, 10485760);

        factory.setConsumerFactory(new DefaultKafkaConsumerFactory<>(props));
        factory.setBatchListener(true);
        factory.setConcurrency(kafkaConcurrency);
        if (virtualThreadsEnabled) {
            factory.getContainerProperties().setListenerTaskExecutor(null);
        }
        factory.setAutoStartup(true);
        return factory;
    }
}