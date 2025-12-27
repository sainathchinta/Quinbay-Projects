package com.gdn.x.product.dao.solr.test.config;

import org.apache.solr.client.solrj.impl.CloudSolrClient;
import org.mockito.Mockito;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.Configuration;

@Configuration
@ComponentScan(basePackages = {"com.gdn.common.base.entity", "com.gdn.x.product.dao"})
public class TestConfig {

  @Bean
  public CloudSolrClient cloudSolrClient() {
    return Mockito.mock(CloudSolrClient.class);
  }
}
