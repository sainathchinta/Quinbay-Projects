package com.gdn.x.mta.distributiontask.service.impl.config;

import org.springframework.boot.context.properties.ConfigurationProperties;

import lombok.Data;

@Data
@ConfigurationProperties(value = "gcs")
public class GcsProperties {
  private String serviceAccountCredentials;

  private String projectId;

  private String sourceImageBucketName;

  private String sourceImageDirectory;

  private String sourceResizeImageDirectory;

  private String pathPrefix;

  private boolean sourceImageEnabled;

  //final image gcs properties
  private boolean finalImageEnabled;

  private boolean fileStoreToGcsMigrationCompleted;

  private String finalImageBucketName;

  private String finalImageDirectory;

  private String finalFullImageDirectory;

  private String finalMediumImageDirectory;

  private String finalThumbnailImageDirectory;

  private String finalSeoulImageDirectory;

}

