package com.gdn.partners.product.analytics.config;

import java.io.ByteArrayInputStream;
import java.io.IOException;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

import com.gdn.partners.product.analytics.properties.GCPProperties;
import com.google.auth.oauth2.ServiceAccountCredentials;
import com.google.cloud.storage.Storage;
import com.google.cloud.storage.StorageOptions;

@Configuration
public class GoogleCloudStoreConfig {

  @Bean
  public Storage googleCloudStorage(GCPProperties gcpProperties) throws IOException {
        String credentialProperties = gcpProperties.getServiceAccountCredentials();
        ServiceAccountCredentials credentials = ServiceAccountCredentials.fromStream(
            new ByteArrayInputStream(credentialProperties.getBytes()));
        return StorageOptions
            .newBuilder()
            .setProjectId(gcpProperties.getStorageProjectId())
            .setCredentials(credentials).build().getService();
  }

  @Bean
  public Storage imageGoogleCloudStorage(GCPProperties gcpProperties) throws IOException {
    String credentialProperties = gcpProperties.getImageServiceAccountCredentials();
    ServiceAccountCredentials credentials = ServiceAccountCredentials.fromStream(
        new ByteArrayInputStream(credentialProperties.getBytes()));
    return StorageOptions.newBuilder().setProjectId(gcpProperties.getStorageProjectId())
        .setCredentials(credentials).build().getService();
  }
}
