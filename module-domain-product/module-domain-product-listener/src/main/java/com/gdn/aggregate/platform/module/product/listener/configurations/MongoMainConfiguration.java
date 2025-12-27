package com.gdn.aggregate.platform.module.product.listener.configurations;

import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.boot.autoconfigure.mongo.MongoProperties;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Primary;
import org.springframework.data.mongodb.MongoDatabaseFactory;
import org.springframework.data.mongodb.core.MongoTemplate;
import org.springframework.data.mongodb.core.SimpleMongoClientDatabaseFactory;
import org.springframework.data.mongodb.repository.config.EnableMongoRepositories;

@Configuration("ProductMongoMainConfiguration")
@EnableConfigurationProperties
@EnableMongoRepositories(
    basePackages = {
        "com.gdn.aggregate.platform.module.product.listener.repository.*"
    },
    mongoTemplateRef = "mainMongoTemplate",
    considerNestedRepositories = true
)
public class MongoMainConfiguration {

  @Primary
  @Bean(name = "mainProperties")
  @ConfigurationProperties(prefix = "spring.data.mongodb")
  public MongoProperties primaryProperties() {
    return new MongoProperties();
  }

  @Primary
  @Bean(name = "mainMongoDBFactory")
  public MongoDatabaseFactory mongoDatabaseFactory(@Qualifier("mainProperties") MongoProperties mongoProperties) {
    return new SimpleMongoClientDatabaseFactory(mongoProperties.getUri());
  }

  @Primary
  @Bean(name = "mainMongoTemplate")
  public MongoTemplate mongoTemplate(@Qualifier("mainMongoDBFactory") MongoDatabaseFactory mongoDatabaseFactory) {
    return new MongoTemplate(mongoDatabaseFactory);
  }
}
