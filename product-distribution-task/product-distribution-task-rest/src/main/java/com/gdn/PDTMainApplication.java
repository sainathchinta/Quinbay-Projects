package com.gdn;

import com.gdn.x.mta.distributiontask.config.MandatoryParameterDefaultProperties;
import com.gdn.x.mta.distributiontask.config.RedisProperties;
import com.gdn.x.mta.distributiontask.service.impl.config.GcsProperties;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.boot.autoconfigure.data.mongo.MongoDataAutoConfiguration;
import org.springframework.boot.autoconfigure.mongo.MongoAutoConfiguration;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.cache.annotation.EnableCaching;
import org.springframework.cloud.openfeign.EnableFeignClients;
import org.springframework.kafka.annotation.EnableKafka;
import org.springframework.scheduling.annotation.EnableAsync;
import org.springframework.transaction.annotation.EnableTransactionManagement;

@EnableFeignClients
@SpringBootApplication(scanBasePackages = {"com.gdn.x.mta.distributiontask", "com.gdn.partners.pdt"}, exclude = {
    MongoAutoConfiguration.class, MongoDataAutoConfiguration.class})
@EnableTransactionManagement
@EnableAsync
@EnableKafka
@EnableCaching
@EnableConfigurationProperties({MandatoryParameterDefaultProperties.class,
    GcsProperties.class, RedisProperties.class})
public class PDTMainApplication {
  public static void main(String[] args) {
    SpringApplication.run(PDTMainApplication.class, args);
  }
}
