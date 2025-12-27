package com.gdn.x.product.service.config;

import com.gdn.x.product.service.interceptor.MandatoryParameterHelper;
import com.mongodb.ClientSessionOptions;
import com.mongodb.client.ClientSession;
import com.mongodb.client.MongoClient;
import com.mongodb.client.MongoDatabase;
import org.springframework.beans.factory.DisposableBean;
import org.springframework.data.mongodb.core.MongoDatabaseFactorySupport;
import org.springframework.data.mongodb.core.MongoExceptionTranslator;

import java.util.Map;

public class MongoRoutingDatabaseFactory extends MongoDatabaseFactorySupport<Map<String, MongoClient>>
    implements DisposableBean {

  private MandatoryParameterHelper mandatoryParameterHelper;

  MongoRoutingDatabaseFactory(Map<String, MongoClient> mongoClient, String databaseName,
      boolean mongoInstanceCreated) {
    super(mongoClient, databaseName, mongoInstanceCreated, new MongoExceptionTranslator());
  }

  MongoRoutingDatabaseFactory(Map<String, MongoClient> mongoClient, String databaseName,
      MandatoryParameterHelper mandatoryParameterHelper) {
    this(mongoClient, databaseName, false);
    this.mandatoryParameterHelper = mandatoryParameterHelper;
  }

  @Override
  public ClientSession getSession(ClientSessionOptions options) {
    return this.getMongoClientInstance().startSession(options);
  }

  @Override
  protected void closeClient() {
    this.getMongoClient().values().forEach(MongoClient::close);
  }

  @Override
  protected MongoDatabase doGetMongoDatabase(String dbName) {
    return this.getMongoClientInstance().getDatabase(dbName);
  }

  private MongoClient getMongoClientInstance() {
    return this.getMongoClient().get(mandatoryParameterHelper.getReadPreference());
  }
}
