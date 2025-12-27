package com.gdn.mta.product.service.config;

import java.util.Set;

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

  private String gcsDomainUrlPath;

  private Set<String> whiteListedChannelIds;

  private boolean finalImageEnabled;

  private boolean fileStoreToGcsMigrationCompleted;

  private String finalImageBucketName;

  private String finalImageDirectory;

  private String finalFullImageDirectory;

  private String finalMediumImageDirectory;

  private String finalThumbnailImageDirectory;

  private String finalSeoulImageDirectory;
}

