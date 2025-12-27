package com.gdn.x.product.service.properties;

import org.springframework.boot.autoconfigure.mongo.MongoProperties;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

@Configuration
public class MongoPropertiesConfiguration {

  @Bean
  @ConfigurationProperties(prefix = "spring.data.secondary.mongodb")
  public MongoProperties mongoSecondaryReadPreferenceProperties() {
    return new MongoProperties();
  }

  @Bean
  @ConfigurationProperties(prefix = "spring.data.primary.mongodb")
  public MongoProperties mongoPrimaryReadPreferenceProperties() {
    return new MongoProperties();
  }

}
