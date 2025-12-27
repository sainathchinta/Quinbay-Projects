package com.gdn.partners.pcu.internal.config;


import java.io.ByteArrayInputStream;
import java.io.IOException;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

import com.gdn.partners.pcu.internal.properties.GCSProperties;
import com.google.auth.oauth2.ServiceAccountCredentials;
import com.google.cloud.storage.Bucket;
import com.google.cloud.storage.Storage;
import com.google.cloud.storage.StorageOptions;

import lombok.extern.slf4j.Slf4j;

@Slf4j
@Configuration
public class GCSConfiguration {

  @Autowired
  private GCSProperties gcsProperties;

  @Bean("googleCloudStorage")
  public Storage googleCloudStorage() throws IOException {
    ServiceAccountCredentials credentials = ServiceAccountCredentials.fromStream(
        new ByteArrayInputStream(gcsProperties.getServiceAccountCredentials().getBytes()));
    return StorageOptions.newBuilder().setProjectId(gcsProperties.getProjectId())
        .setCredentials(credentials).build().getService();
  }

  @Bean("bulkBucket")
  public Bucket bulkBucket() throws IOException {
    return googleCloudStorage().get(gcsProperties.getBucketName(), Storage.BucketGetOption.fields());
  }

  @Bean("sourceImageBucket")
  public Bucket sourceImageBucket() throws IOException {
    return googleCloudStorage().get(gcsProperties.getSourceImageBucketName(), Storage.BucketGetOption.fields());
  }

  @Bean("finalImageBucket")
  public Bucket finalImageBucket() throws IOException {
    return googleCloudStorage()
        .get(gcsProperties.getFinalImageBucketName(), Storage.BucketGetOption.fields());
  }

  @Bean("brandBucket")
  public Bucket brandBucket() throws IOException {
    return googleCloudStorage().get(gcsProperties.getBrandBucketName(), Storage.BucketGetOption.fields());
  }

  @Bean("attributeBucket")
  public Bucket attributeBucket() throws IOException {
    return googleCloudStorage().get(gcsProperties.getAttributeBucketName(),
        Storage.BucketGetOption.fields());
  }
}
