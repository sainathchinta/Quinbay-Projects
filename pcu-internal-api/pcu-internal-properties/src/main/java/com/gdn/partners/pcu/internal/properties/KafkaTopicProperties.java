package com.gdn.partners.pcu.internal.properties;

import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.stereotype.Component;

import lombok.Data;

@Data
@ConfigurationProperties(value = "kafka.topic")
@Component
public class KafkaTopicProperties {

  private String deleteAutoApprovedProductEventName;
  private String internalHistoryEventName;
  private String retryFinalQcEventName;
  private String bulkReviewUploadEvent;
  private String productAttributeFeedbackEventName;
}
