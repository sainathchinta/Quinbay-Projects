package com.gdn.partners.product.analytics.repository;

import java.util.Optional;

import com.mongodb.client.MongoClient;
import com.mongodb.client.MongoClients;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.Configuration;
import org.springframework.data.domain.AuditorAware;
import org.springframework.data.mongodb.config.AbstractMongoClientConfiguration;
import org.springframework.data.mongodb.core.MongoTemplate;
import org.springframework.data.mongodb.repository.config.EnableMongoRepositories;

import com.fasterxml.jackson.databind.ObjectMapper;
import org.testcontainers.containers.MongoDBContainer;

/**
 * @author Pradeep Reddy
 */
@Configuration
@EnableMongoRepositories(basePackages = {"com.gdn.partners.product.analytics.repository"})
@ComponentScan(basePackages = {"com.gdn.partners.product.analytics.entity"})
public class TestConfig extends AbstractMongoClientConfiguration {

  private static final String COM_GDN_PRODUCT_ANALYTICS = "com.gdn.partners.product.analytics.entity";
  private static final String PRODUCT_ANALYTICS = "product-analytics";

  @Override
  protected String getDatabaseName() {
    return TestConfig.PRODUCT_ANALYTICS;
  }


  @Bean
  protected ObjectMapper objectMapper() {
    return new ObjectMapper();
  }

  @Bean
  public MongoTemplate mongoTemplate() throws Exception {
    return new MongoTemplate(this.mongoClient(), TestConfig.PRODUCT_ANALYTICS);
  }

  @Bean
  public MongoClient mongoClient() {
    MongoDBContainer mongoDBContainer = new MongoDBContainer("mongo:6.0.0");
    mongoDBContainer.start();

    return MongoClients.create(mongoDBContainer.getReplicaSetUrl());
  }

  @Bean
  public AuditorAware<String> stringAuditorAware() {
    return new AuditorAware<String>() {
      @Override
      public Optional<String> getCurrentAuditor() {
        return Optional.of("system");
      }
    };
  }
}
