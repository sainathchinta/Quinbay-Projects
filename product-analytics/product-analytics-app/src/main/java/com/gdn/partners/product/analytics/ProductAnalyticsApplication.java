package com.gdn.partners.product.analytics;

/**
 * @author Pradeep Reddy
 */
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.cache.annotation.EnableCaching;
import org.springframework.cloud.openfeign.EnableFeignClients;
import org.springframework.cloud.netflix.hystrix.EnableHystrix;
import org.springframework.data.mongodb.repository.config.EnableMongoRepositories;
import org.springframework.retry.annotation.EnableRetry;
import org.springframework.scheduling.annotation.EnableAsync;

import com.gdn.partners.product.analytics.properties.ApplicationProperties;
import com.gdn.partners.product.analytics.properties.ClientParameterProperties;
import com.gdn.partners.product.analytics.properties.FileHelperProperties;
import com.gdn.partners.product.analytics.properties.GCPProperties;
import com.gdn.partners.product.analytics.properties.SubmitBigQueryProcessLockProperties;


@EnableMongoRepositories
@SpringBootApplication
@EnableRetry
@EnableFeignClients
@EnableHystrix
@EnableAsync
@EnableCaching
@EnableConfigurationProperties({ClientParameterProperties.class, ApplicationProperties.class, GCPProperties.class,
    FileHelperProperties.class, SubmitBigQueryProcessLockProperties.class})
public class ProductAnalyticsApplication {

  public static void main(String[] args) {
    SpringApplication.run(ProductAnalyticsApplication.class, args);
  }
}
