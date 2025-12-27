package com.gdn.partners.pcu.external.properties;

import org.springframework.boot.context.properties.ConfigurationProperties;

import lombok.Data;

@Data
@ConfigurationProperties(value = "gcs")
public class GCSProperties {
  private String serviceAccountCredentials;

  private String bucketName;

  private String projectId;

  private String templateDirectory;

  private String vatUploadPath;

  private String dataUploadPath;

  private boolean enabled;

  // source image properties
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

  //Attribute GCS Properties
  private String attributeBucketName;

  private String attributePathPrefix;

  private String attributeImageDirectory;

  // Brand GCS Properties
  private boolean brandGcsEnabled;

  private String brandBucketName;

  private String brandSourceDirectory;

  private String brandFinalDirectory;

  private String brandLogoDirectory;

  private String brandProfileBannerDirectory;

  //IPR Files
  private String iprBucket;

  private String iprDataUploadPath;

}