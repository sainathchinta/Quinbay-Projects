package com.gdn.x.product.service.config;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.autoconfigure.mongo.MongoProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.data.mongodb.core.MongoTemplate;

import com.mongodb.ReadPreference;
import com.mongodb.WriteConcern;
import com.mongodb.client.MongoClient;

import lombok.extern.slf4j.Slf4j;

@Slf4j
@Configuration
public class MongoTemplateConfiguration {

  @Autowired
  private MongoProperties mongoSecondaryReadPreferenceProperties;

  @Autowired
  private MongoProperties mongoPrimaryReadPreferenceProperties;

  @Autowired
  private MongoClient mongoClient;

  @Autowired
  private MongoClient primaryReadMongoClient;

  @Value("${spring.data.secondary.mongodb.readPreference}")
  private String secondaryReadPreference;

  @Value("${spring.data.primary.mongodb.readPreference}")
  private String primaryReadPreference;

  @Value("${spring.data.secondary.mongodb.write.concern.replica.count}")
  private int secondaryWriteConcernReplicaCount;

  @Value("${spring.data.primary.mongodb.write.concern.replica.count}")
  private int primaryWriteConcernReplicaCount;

  @Value("${spring.data.secondary.mongodb.write.concern.replica.timeout}")
  private int secondaryWriteConcernReplicaTimeout;

  @Value("${spring.data.primary.mongodb.write.concern.replica.timeout}")
  private int primaryWriteConcernReplicaTimeout;

  @Bean("SECONDARY_PREFERRED")
  public MongoTemplate secondaryReadMongoTemplate() {
    MongoTemplate mongoTemplate = new MongoTemplate(mongoClient, mongoSecondaryReadPreferenceProperties.getDatabase());
    mongoTemplate.setReadPreference(ReadPreference.valueOf(secondaryReadPreference));
    mongoTemplate.setWriteConcern(new WriteConcern(secondaryWriteConcernReplicaCount, secondaryWriteConcernReplicaTimeout));
    return mongoTemplate;
  }

  @Bean("PRIMARY_PREFERRED")
  public MongoTemplate primaryReadMongoTemplate() {
    MongoTemplate mongoTemplate = new MongoTemplate(primaryReadMongoClient, mongoPrimaryReadPreferenceProperties.getDatabase());
    mongoTemplate.setReadPreference(ReadPreference.valueOf(primaryReadPreference));
    mongoTemplate.setWriteConcern(new WriteConcern(primaryWriteConcernReplicaCount, primaryWriteConcernReplicaTimeout));
    return mongoTemplate;
  }
}
