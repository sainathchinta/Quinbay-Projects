package com.gdn.x.productcategorybase.config;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

import com.gdn.x.productcategorybase.properties.AggregatorProperties;
import com.gdn.x.productcategorybase.service.config.ApplicationConfig;

@Configuration
public class MapperConfiguration {

  @Value("${applicationconfig.regenerateWhenDeleteAllowedAttribute}")
  private boolean regenerateWhenDeleteAllowedAttribute;

  @Value("${domainEvent.publisher.aggregator.pageSize}")
  private int domainEventPublisherAggregatorPageSize;

  @Value("${domainEvent.publisher.aggregator.totalData}")
  private int domainEventPublisherAggregatorTotalData;

  @Bean
  public ApplicationConfig applicationConfig() {
    ApplicationConfig applicationConfig = new ApplicationConfig();
    applicationConfig.setRegenerateWhenDeleteAllowedAttribute(regenerateWhenDeleteAllowedAttribute);
    return applicationConfig;
  }

  @Bean
  public AggregatorProperties aggregateProperties() {
    return new AggregatorProperties(domainEventPublisherAggregatorPageSize, domainEventPublisherAggregatorTotalData);
  }

}
