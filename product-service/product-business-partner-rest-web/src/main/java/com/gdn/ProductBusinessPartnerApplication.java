package com.gdn;

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

import com.gdn.mta.product.service.config.GcsProperties;
import com.gdn.mta.product.service.config.KafkaTopicProperties;
import com.gdn.pbp.property.MandatoryParameterDefaultProperties;
import com.gdn.pbp.property.VersionProperties;


@EnableFeignClients(basePackages = "com.gdn.partners.pbp.outbound")
@EnableCaching
@SpringBootApplication(scanBasePackages = {"com.gdn.mta.product", "com.gdn.partners.product.orchestrator",
    "com.gdn.pbp.config", "com.gdn.x.base.controller", "com.gdn.pbp.property", "com.gdn.partners.pbp", "com.gdn.partner.pbp",
    "com.gdn.partners.pbp.service.productlevel1", "com.gdn.pbp.property", "com.gdn.mta.product.config"}
    , exclude = {MongoAutoConfiguration.class, MongoDataAutoConfiguration.class})
@EnableTransactionManagement
@EnableAsync
@EnableKafka
@EnableConfigurationProperties({MandatoryParameterDefaultProperties.class, VersionProperties.class,
    GcsProperties.class, KafkaTopicProperties.class})
public class ProductBusinessPartnerApplication {

  public static void main(String[] args) {
    SpringApplication.run(ProductBusinessPartnerApplication.class, args);
  }
}