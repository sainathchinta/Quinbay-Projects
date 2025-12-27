package com.gdn.x.product.service.config;

import java.util.Optional;

import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.autoconfigure.mongo.MongoProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.FilterType;
import org.springframework.context.annotation.Primary;
import org.springframework.data.domain.AuditorAware;
import org.springframework.data.mongodb.MongoDatabaseFactory;
import org.springframework.data.mongodb.config.AbstractMongoClientConfiguration;
import org.springframework.data.mongodb.config.EnableMongoAuditing;
import org.springframework.data.mongodb.repository.MongoRepository;
import org.springframework.data.mongodb.repository.config.EnableMongoRepositories;

import com.gdn.x.product.service.constants.MongoReadPreference;
import com.gdn.x.product.service.interceptor.MandatoryParameterHelper;
import com.google.common.collect.ImmutableMap;
import com.mongodb.ConnectionString;
import com.mongodb.MongoClientSettings;
import com.mongodb.client.MongoClient;
import com.mongodb.client.MongoClients;
import lombok.extern.slf4j.Slf4j;

@Slf4j
@Configuration
@EnableMongoRepositories(basePackages = {
    "com.gdn.x.product.dao.api", "com.gdn.x.product.dao.impl"},
    includeFilters = @ComponentScan.Filter(
        type = FilterType.ASSIGNABLE_TYPE, classes = {MongoRepository.class}))
@EnableMongoAuditing(auditorAwareRef = "currentAuditor")
public class MongoClientConfiguration extends AbstractMongoClientConfiguration {

  @Autowired
  private MongoProperties mongoSecondaryReadPreferenceProperties;

  @Autowired
  private MongoProperties mongoPrimaryReadPreferenceProperties;

  @Autowired
  private MandatoryParameterHelper mandatoryParameterHelper;

  @Override
  @Bean
  @Primary
  public MongoClient mongoClient() {
    return MongoClients.create(getMongoClientSettings(mongoSecondaryReadPreferenceProperties));
  }

  @Bean
  public MongoClient primaryReadMongoClient() {
    return MongoClients.create(getMongoClientSettings(mongoPrimaryReadPreferenceProperties));
  }

  @Bean
  public AuditorAware<String> currentAuditor() {
    return () -> Optional.of(
        StringUtils.defaultIfBlank(mandatoryParameterHelper.getUsername(), "system"));
  }

  @Override
  public MongoDatabaseFactory mongoDbFactory() {
    return new MongoRoutingDatabaseFactory(ImmutableMap.of(MongoReadPreference.SECONDARY_PREFERRED.name(), this.mongoClient(),
        MongoReadPreference.PRIMARY_PREFERRED.name(), primaryReadMongoClient()), this.getDatabaseName(),
        mandatoryParameterHelper);
  }

  @Override
  protected String getDatabaseName() {
    return mongoSecondaryReadPreferenceProperties.getDatabase();
  }


  private MongoClientSettings getMongoClientSettings(MongoProperties mongoProperties) {
    MongoClientSettings mongoClientSettings = MongoClientSettings.builder().applyConnectionString(new ConnectionString(mongoProperties.getUri())).build();
    return mongoClientSettings;
  }
}
