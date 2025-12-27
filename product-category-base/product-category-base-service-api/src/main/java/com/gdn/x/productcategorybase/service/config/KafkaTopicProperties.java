package com.gdn.x.productcategorybase.service.config;

import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.stereotype.Component;
import lombok.Data;

@Data
@ConfigurationProperties(value = "configuration.kafka.topic")
@Component
public class KafkaTopicProperties {

  // terminated seller cleanup
  private String terminatedSellerSkuCleanup;
  private String terminatedSellerSkuCleanupStatus;
  private String terminatedSellerSkuImageCleanup;

  // restricted keyword history event
  private String restrictedKeywordHistoryEvent;

  // category update history event
  private String categoryUpdateHistoryEvent;

  private boolean autoStartup;

  private String sizeChartUpdateEventName;

  private String brandHistoryEvent;
  private String brandAuthActivateEvent;
  private String brandAuthApproveEmailEvent;
  private String brandAuthRejectEmailEvent;
  private String brandAuthNeedRevisionEmailEvent;
  private String brandAuthNearExpiryEmailEvent;

  //Attribute Data BackFilling Events
  private String pbpAttributeMigrationEvent;
  private String productAttributeMigrationTopic;
  private String productDsAttributeExtractionEvent;
  private String publishVendorEvent;
  private String productInternalHistoryEvent;

  // Hard Delete mfd true Attribute and Image Entries
  private String deleteMfdTrueImageAndAttributeEvent;
  private String compressedVideoUpdateEvent;

  // external history event
  private String productSkuUpdateExternalHistoryEvent;

  private String categoryHierarchyCaffeineCacheEvictEvent;
}
