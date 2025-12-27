package com.gdn.micro.graphics;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.EnableAutoConfiguration;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.boot.autoconfigure.data.mongo.MongoDataAutoConfiguration;
import org.springframework.boot.autoconfigure.jdbc.DataSourceAutoConfiguration;
import org.springframework.boot.autoconfigure.mongo.MongoAutoConfiguration;
import org.springframework.boot.autoconfigure.orm.jpa.HibernateJpaAutoConfiguration;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.cloud.openfeign.EnableFeignClients;
import org.springframework.kafka.annotation.EnableKafka;
import org.springframework.retry.annotation.EnableRetry;
import org.springframework.scheduling.annotation.EnableAsync;

import com.gdn.micro.graphics.service.config.GcsProperties;
import com.gdn.micro.graphics.web.model.MandatoryParameterDefaultProperties;

@EnableAsync
@EnableRetry
@EnableKafka
@EnableFeignClients
@EnableAutoConfiguration(exclude = {DataSourceAutoConfiguration.class, HibernateJpaAutoConfiguration.class,
    MongoAutoConfiguration.class, MongoDataAutoConfiguration.class})
@SpringBootApplication(scanBasePackages = {"com.gdn.micro.graphics"})
@EnableConfigurationProperties({GcsProperties.class, MandatoryParameterDefaultProperties.class})
public class GraphicsProcessorApplication {

  public static void main(String[] args) {
    SpringApplication.run(GraphicsProcessorApplication.class, args);
  }
}