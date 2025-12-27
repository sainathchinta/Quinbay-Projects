package com.gdn.micro.graphics.service.config;

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

  private String resizeImagePrefix;

  //final image gcs properties
  private boolean finalImageEnabled;

  private boolean fileStoreToGcsMigrationCompleted;

  private String finalImageBucketName;

  private String finalImageDirectory;

  private String finalFullImageDirectory;

  private String finalMediumImageDirectory;

  private String finalThumbnailImageDirectory;

  private String finalSeoulImageDirectory;

  //xrma gcs changes properties
  private String rmaBucketName;
  private String rmaClientId;
  private String rmaTemporaryImageSourcePath;
  private String rmaTemporaryImageDestinationPath;
  private String rmaGcsPath;
  private boolean scaleGcsEnabled;


  //oxford gcs changes properties
  private String oxfordBucketName;
  private String oxfordClientId;
  private String oxfordTemporaryImageSourcePath;
  private String oxfordTemporaryImageDestinationPath;
  private String oxfordGcsPath;
  private String removeGcsPathForOxford;
  private boolean storeGcsEnabled;
  private boolean isDisplayGcsEnabled;
  private boolean isRemoveGcsEnabled;
  private boolean isRMAImageMigrationDone;
  private boolean isOxfordImageMigrationDone;

  //order gcs properties
  private String orderBucketName;
  private String orderTemporaryImageSourcePath;
  private String orderTemporaryImageDestinationPath;
  private String orderClientId;
  private String orderResizePath;

  private String activeProductNewImageClientId;
}

