package com.gdn.mta.product.config;

import feign.Request;
import feign.RetryableException;
import feign.Retryer;
import lombok.extern.slf4j.Slf4j;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

@Configuration
@Slf4j
@ConditionalOnProperty(name = "custom.feign.retry.bean.creation", havingValue = "true")
public class FeignClientConfiguration {

    @Value("${pbp.feign.retry.disabled}")
    private boolean feignRetryEnabled;

    @Value("${pbp.feign.retry.maxAttempts}")
    private int maxAttempts;

    @Value("${pbp.feign.retry.maxInterval}")
    private long maxInterval;

    private static final Logger LOGGER = LoggerFactory.getLogger(FeignClientConfiguration.class);

    @Bean
    @ConditionalOnProperty(name = "pbp.feign.retry.disabled", havingValue = "false")
    public Retryer feignRetryer() {
        return new Retryer.Default(100, maxInterval, maxAttempts) {
            private int retries = 0;

            @Override
            public void continueOrPropagate(RetryableException e) {
                Request request = e.request();
                LOGGER.error("Retry attempt count was : {} for API : {} ", ++retries,
                  request.requestTemplate().url());
                super.continueOrPropagate(e);
            }
        };
    }

    @Bean
    @ConditionalOnProperty(name = "pbp.feign.retry.disabled", havingValue = "true")
    public Retryer defaultFeignRetryer() {
        return Retryer.NEVER_RETRY;
    }
}
