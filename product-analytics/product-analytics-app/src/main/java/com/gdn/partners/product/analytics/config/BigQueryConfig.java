package com.gdn.partners.product.analytics.config;

import java.io.ByteArrayInputStream;
import java.io.IOException;

import org.apache.velocity.app.VelocityEngine;
import org.apache.velocity.runtime.RuntimeConstants;
import org.apache.velocity.runtime.resource.loader.ClasspathResourceLoader;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

import com.gdn.partners.product.analytics.properties.GCPProperties;
import com.google.auth.oauth2.ServiceAccountCredentials;
import com.google.cloud.bigquery.BigQuery;
import com.google.cloud.bigquery.BigQueryOptions;

@Configuration
public class BigQueryConfig {

  private static final String CLASSPATH = "classpath";
  private static final String RESOURCE_LOADER_CLASS = "classpath.resource.loader.class";

  @Bean
  public BigQuery bigQuery(GCPProperties gcpProperties) throws Exception {
    ServiceAccountCredentials credentials = ServiceAccountCredentials
        .fromStream(new ByteArrayInputStream(gcpProperties.getServiceAccountCredentials().getBytes()));

    return BigQueryOptions.newBuilder().setCredentials(credentials)
        .setProjectId(gcpProperties.getProjectId()).build().getService();
  }

  @Bean
  public VelocityEngine velocityEngine() {
    VelocityEngine velocityEngine = new VelocityEngine();
    velocityEngine.setProperty(RuntimeConstants.RESOURCE_LOADER, CLASSPATH);
    velocityEngine.setProperty(RESOURCE_LOADER_CLASS, ClasspathResourceLoader.class.getName());
    velocityEngine.init();
    return velocityEngine;
  }


}
