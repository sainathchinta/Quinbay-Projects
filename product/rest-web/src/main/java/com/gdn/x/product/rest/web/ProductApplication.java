package com.gdn.x.product.rest.web;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.EnableAutoConfiguration;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.boot.autoconfigure.data.mongo.MongoDataAutoConfiguration;
import org.springframework.boot.autoconfigure.data.redis.RedisAutoConfiguration;
import org.springframework.boot.autoconfigure.domain.EntityScan;
import org.springframework.boot.autoconfigure.freemarker.FreeMarkerAutoConfiguration;
import org.springframework.boot.autoconfigure.jdbc.DataSourceAutoConfiguration;
import org.springframework.boot.autoconfigure.mongo.MongoAutoConfiguration;
import org.springframework.boot.autoconfigure.web.servlet.WebMvcAutoConfiguration;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.cache.annotation.EnableCaching;
import org.springframework.cloud.openfeign.EnableFeignClients;
import org.springframework.kafka.annotation.EnableKafka;
import org.springframework.scheduling.annotation.EnableAsync;
import org.springframework.transaction.annotation.EnableTransactionManagement;

import com.gdn.x.product.rest.web.properties.PrimaryDataSourceApis;

@SpringBootApplication(scanBasePackages = {"com.gdn.x.product"})
@EnableTransactionManagement
@EnableAsync
@EnableCaching
@EnableKafka
@EntityScan(basePackages = "com.gdn.x.product.model.entity")
@EnableAutoConfiguration(exclude={DataSourceAutoConfiguration.class,
  WebMvcAutoConfiguration.class, FreeMarkerAutoConfiguration.class, RedisAutoConfiguration.class,
    MongoAutoConfiguration.class, MongoDataAutoConfiguration.class})
@EnableFeignClients(basePackages = "com.gdn.x.product.outbound.api.feign")
@EnableConfigurationProperties(value = {PrimaryDataSourceApis.class})
public class ProductApplication {
  public static void main(String[] args) {
    SpringApplication.run(ProductApplication.class, args);
  }
}