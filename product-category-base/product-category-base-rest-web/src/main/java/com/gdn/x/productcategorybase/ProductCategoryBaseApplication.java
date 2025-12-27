package com.gdn.x.productcategorybase;

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

import com.gdn.x.productcategorybase.properties.GcsProperties;

@EnableKafka
@EnableFeignClients
@SpringBootApplication(scanBasePackages = {"com.gdn.x.productcategorybase"},
    exclude = {MongoAutoConfiguration.class, MongoDataAutoConfiguration.class})
@EnableTransactionManagement
@EnableAsync
@EnableCaching
@EnableConfigurationProperties({GcsProperties.class})
public class ProductCategoryBaseApplication {
  public static void main(String[] args) {
    SpringApplication.run(ProductCategoryBaseApplication.class, args);
  }
}
