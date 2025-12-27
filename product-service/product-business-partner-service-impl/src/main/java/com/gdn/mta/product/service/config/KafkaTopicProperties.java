package com.gdn.mta.product.service.config;

import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.stereotype.Component;

import lombok.Data;

@Data
@ConfigurationProperties(value = "configuration.kafka.topic")
@Component
public class KafkaTopicProperties {
  // Image Resize event
  private String imageResizeEventNoPriority;
  private String imageResizeEventPriority1;
  private String imageResizeEventPriority2;

  // Image Resize status event
  private String imageResizeStatusEventNoPriority;
  private String imageResizeStatusEventPriority1;
  private String imageResizeStatusEventPriority2;

  // Vendor combined event
  private String vendorCombinedEventNoPriority;
  private String vendorCombinedEventPriority1;
  private String vendorCombinedEventPriority2;

  // Vendor approval event
  private String vendorApprovalEventNoPriority;
  private String vendorApprovalEventPriority1;
  private String vendorApprovalEventPriority2;

  // Image scale status event
  private String imageScaleStatusEventNoPriority;
  private String imageScaleStatusEventPriority1;
  private String imageScaleStatusEventPriority2;

  // terminated seller cleanup
  private String terminatedSellerSkuCleanup;
  private String terminatedSellerSkuCleanupStatus;

  private String addDeleteVariantRetryPublishEvent;

  private boolean autoStartup;

  // Generic kafka event
  private String genericKafkaEventName;
  private String genericKafkaGroupId;
  private String genericKafkaEventConcurrency;

  private String productSkuSolrUpdateEvent;

  private String pbpAttributeMigrationEvent;
  private String xProductAttributeMigrationEvent;
  private String pcbVendorPublishEvent;

  private String populateL3HistoryByProductCodeEvent;
}

