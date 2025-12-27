package com.gdn.micro.graphics.service.config;

import java.io.ByteArrayInputStream;
import java.io.IOException;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

import com.google.auth.oauth2.ServiceAccountCredentials;
import com.google.cloud.storage.Bucket;
import com.google.cloud.storage.Storage;
import com.google.cloud.storage.StorageOptions;

@Configuration
public class GCSConfiguration {

  @Autowired
  private GcsProperties gcsProperties;

  @Bean("googleCloudStorage")
  public Storage googleCloudStorage() throws IOException {
    ServiceAccountCredentials credentials = ServiceAccountCredentials.fromStream(
        new ByteArrayInputStream(gcsProperties.getServiceAccountCredentials().getBytes()));
    return StorageOptions.newBuilder().setProjectId(gcsProperties.getProjectId()).setCredentials(credentials).build()
        .getService();
  }

  @Bean("sourceImageBucket")
  public Bucket sourceImageBucket() throws IOException {
    return googleCloudStorage().get(gcsProperties.getSourceImageBucketName(), Storage.BucketGetOption.fields());
  }

  @Bean("finalImageBucket")
  public Bucket finalImageBucket() throws IOException {
    return googleCloudStorage().get(gcsProperties.getFinalImageBucketName(), Storage.BucketGetOption.fields());
  }

  @Bean("rmaImageBucket")
  public Bucket rmaImageBucket() throws IOException {
    return googleCloudStorage().get(gcsProperties.getRmaBucketName(), Storage.BucketGetOption.fields());
  }

  @Bean("oxfordImageBucket")
  public Bucket oxfordImageBucket() throws IOException {
    return googleCloudStorage().get(gcsProperties.getOxfordBucketName(), Storage.BucketGetOption.fields());
  }

  @Bean("orderImageBucket")
  public Bucket orderImageBucket() throws IOException {
    return googleCloudStorage().get(gcsProperties.getOrderBucketName(),
      Storage.BucketGetOption.fields());
  }
}

