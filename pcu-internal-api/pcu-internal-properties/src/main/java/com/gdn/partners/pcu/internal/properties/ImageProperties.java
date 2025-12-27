package com.gdn.partners.pcu.internal.properties;

import org.springframework.boot.context.properties.ConfigurationProperties;

import lombok.Data;

@Data
@ConfigurationProperties(value = "image")
public class ImageProperties {

  private String sourceDirectory;
  private int maxReviewSize;

  // Properties for image upload
  private String seoul;
  private String basePath;
  private String fullPath;
  private String mediumPath;
  private String thumbnailPath;
}
