package com.gdn.mta.bulk;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.boot.autoconfigure.data.elasticsearch.ElasticsearchDataAutoConfiguration;
import org.springframework.boot.autoconfigure.data.mongo.MongoDataAutoConfiguration;
import org.springframework.boot.autoconfigure.elasticsearch.ElasticsearchRestClientAutoConfiguration;
import org.springframework.boot.autoconfigure.mongo.MongoAutoConfiguration;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.cache.annotation.EnableCaching;
import org.springframework.cloud.openfeign.EnableFeignClients;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.kafka.annotation.EnableKafka;
import org.springframework.retry.annotation.EnableRetry;
import org.springframework.scheduling.annotation.EnableAsync;
import org.springframework.transaction.annotation.EnableTransactionManagement;

import com.gdn.mta.bulk.property.MandatoryParameterDefaultProperties;

@EnableFeignClients
@SpringBootApplication(scanBasePackages = {"com.gdn.mta.bulk"})
@ComponentScan("com.gdn.mta.bulk.*")
@EnableTransactionManagement
@EnableAsync
@EnableKafka
@EnableRetry
@EnableCaching
@EnableConfigurationProperties(value = {MandatoryParameterDefaultProperties.class})
public class BulkProcessorApplication {
  public static void main(String[] args) {
    SpringApplication.run(BulkProcessorApplication.class, args);
  }

}
