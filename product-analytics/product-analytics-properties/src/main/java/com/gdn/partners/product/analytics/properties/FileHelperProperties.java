package com.gdn.partners.product.analytics.properties;

import org.springframework.boot.context.properties.ConfigurationProperties;

import lombok.Data;

@Data
@ConfigurationProperties(value = "helper")
public class FileHelperProperties {

  private int bufferSize = 1024;
  private String directory;
  private int batchSizeForImageDeletion;
}
