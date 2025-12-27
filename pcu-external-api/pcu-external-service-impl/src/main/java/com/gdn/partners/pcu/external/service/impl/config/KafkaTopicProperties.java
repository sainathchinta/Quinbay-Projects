package com.gdn.partners.pcu.external.service.impl.config;

import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.stereotype.Component;

import lombok.Data;

@Data
@ConfigurationProperties(value = "configuration.kafka.topic")
@Component
public class KafkaTopicProperties {

  private String bulkDownloadAllEvent;
  private String generateQrCodeStore;
  private String generateQrCodeProduct;
  private String xProductAutoHealEvent;
  private String pbpAutoFixHistory;
}
