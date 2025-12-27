package com.gdn.partners.pcu.external.properties;

import org.springframework.boot.context.properties.ConfigurationProperties;

import lombok.Data;

@Data
@ConfigurationProperties("image")
public class ImageProperties {

  private String seoul;

  private String basePath;

  private String fullPath;

  private String mediumPath;

  private String thumbnailPath;

}
