package com.gdn.partners.product.analytics.config;

import com.gdn.partners.product.analytics.client.helper.ClientParameterHelper;
import com.mongodb.client.MongoClient;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.data.domain.AuditorAware;
import org.springframework.data.mongodb.config.EnableMongoAuditing;

import java.util.Optional;


@Configuration
@EnableMongoAuditing(auditorAwareRef = "currentAuditor")
public class MongoConfiguration {

  @Autowired
  private MongoClient mongoClient;

  @Autowired
  private ClientParameterHelper clientParameterHelper;

  @Bean
  public AuditorAware<String> currentAuditor() {
    return () -> Optional.of(
        StringUtils.defaultIfBlank(clientParameterHelper.getUsername(), "system"));
  }

}
