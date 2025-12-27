package com.gdn.mta.bulk.config;

import java.io.ByteArrayInputStream;
import java.io.IOException;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

import com.google.api.client.http.javanet.NetHttpTransport;
import com.google.api.client.json.jackson2.JacksonFactory;
import com.google.api.services.youtube.YouTube;
import com.google.auth.oauth2.ServiceAccountCredentials;
import com.google.cloud.http.HttpTransportOptions;
import com.google.cloud.storage.Bucket;
import com.google.cloud.storage.Storage;
import com.google.cloud.storage.StorageOptions;

@Configuration
public class BeanConfiguration {

  @Value("${gcs.project.id}")
  private String gcsProjectId;

  @Value("${gcs.service.account.credentials}")
  private String gcsServiceAccountCredentials;

  @Value("${gcs.bucket.name}")
  private String gcsBulkBucketName;

  @Value("${gcs.pricing.bucket.name}")
  private String gcsPricingBucketName;

  @Value("${gcs.source.image.bucket.name}")
  private String gcsSourceImageBucketName;

  @Value("${gcs.timeout}")
  private Integer gcsTimeout;

  @Value("${gcs.final.image.bucket.name}")
  private String gcsFinalImageBulkBucketName;

  private static final String APP_ID = "BULK";

  @Bean("googleCloudStorage")
  public Storage googleCloudStorage() throws IOException {
    ServiceAccountCredentials credentials =
        ServiceAccountCredentials.fromStream(new ByteArrayInputStream(gcsServiceAccountCredentials.getBytes()));
    HttpTransportOptions transportOptions = StorageOptions.getDefaultHttpTransportOptions();
    transportOptions =
      transportOptions.toBuilder().setConnectTimeout(gcsTimeout).setReadTimeout(gcsTimeout).build();
    return StorageOptions.newBuilder().setProjectId(gcsProjectId).setCredentials(credentials)
      .setTransportOptions(transportOptions).build().getService();
  }

  @Bean("bulkBucket")
  public Bucket bulkBucket() throws IOException {
    return googleCloudStorage().get(gcsBulkBucketName, Storage.BucketGetOption.fields());
  }

  @Bean("finalImageBucket")
  public Bucket finalImageBucket() throws IOException {
    return googleCloudStorage().get(gcsFinalImageBulkBucketName, Storage.BucketGetOption.fields());
  }

  @Bean("pricingBulkBucket")
  public Bucket pricingBulkBucket() throws IOException {
    return googleCloudStorage().get(gcsPricingBucketName, Storage.BucketGetOption.fields());
  }

  @Bean("sourceImageBucket")
  public Bucket sourceImageBucket() throws IOException {
    return googleCloudStorage().get(gcsSourceImageBucketName, Storage.BucketGetOption.fields());
  }

  @Bean("youTube")
  public static YouTube youTube() {
    return new YouTube.Builder(new NetHttpTransport(), new JacksonFactory(), request -> {
    }).setApplicationName(APP_ID).build();
  }

  public void setGcsProjectId(String gcsProjectId) {
    this.gcsProjectId = gcsProjectId;
  }

  public void setGcsServiceAccountCredentials(String gcsServiceAccountCredentials) {
    this.gcsServiceAccountCredentials = gcsServiceAccountCredentials;
  }

  public void setGcsBulkBucketName(String gcsBulkBucketName) {
    this.gcsBulkBucketName = gcsBulkBucketName;
  }

  public void setGcsSourceImageBucketName(String gcsSourceImageBucketName) {
    this.gcsSourceImageBucketName = gcsSourceImageBucketName;
  }
}
