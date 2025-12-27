package com.gdn.aggregate.platform.module.product.listener.configurations;

import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.boot.autoconfigure.mongo.MongoProperties;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.data.mongodb.MongoDatabaseFactory;
import org.springframework.data.mongodb.core.MongoTemplate;
import org.springframework.data.mongodb.core.SimpleMongoClientDatabaseFactory;
import org.springframework.data.mongodb.repository.config.EnableMongoRepositories;

@Configuration("ProductMongoSubConfiguration")
@EnableConfigurationProperties
@EnableMongoRepositories(
    basePackages = {
        "com.gdn.aggregate.platform.module.product.listener.repositorysub.*"
    },
    mongoTemplateRef = "subMongoTemplate"
)
public class MongoSubConfiguration {

  @Bean(name = "subProperties")
  @ConfigurationProperties(prefix = "spring.data.mongodb.sub")
  public MongoProperties secondaryProperties() {
    return new MongoProperties();
  }

  @Bean(name = "subMongoDBFactory")
  public MongoDatabaseFactory mongoDatabaseFactory(@Qualifier("subProperties") MongoProperties mongoProperties) {
    return new SimpleMongoClientDatabaseFactory(mongoProperties.getUri());
  }

  @Bean(name = "subMongoTemplate")
  public MongoTemplate mongoTemplate(@Qualifier("subMongoDBFactory") MongoDatabaseFactory mongoDatabaseFactory) {
    return new MongoTemplate(mongoDatabaseFactory);
  }

}
