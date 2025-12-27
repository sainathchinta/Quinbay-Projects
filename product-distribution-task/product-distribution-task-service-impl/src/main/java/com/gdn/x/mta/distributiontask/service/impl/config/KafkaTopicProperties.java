package com.gdn.x.mta.distributiontask.service.impl.config;

import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.stereotype.Component;

import lombok.Data;

@Data
@ConfigurationProperties(value = "configuration.kafka.topic")
@Component
public class KafkaTopicProperties {
  // Update to vendor solr event
  private String pdtProductCombinedUpdateToSolrEvent;

  //vendor approval events
  private String productVendorApprovedTaskEvent;
  private String productVendorApprovedTaskPriority1Event;
  private String productVendorApprovedTaskPriority2Event;
  private String editedProductVendorApprovedTaskEvent;
  private String revisedProductVendorApprovedTaskEvent;
  private String productScreeningApprovedTaskEvent;

  //internal history event
  private String internalHistoryEventName;

  //permanent delete event
  private String permanentDeleteProductResult;

  // Auto start up
  private boolean autoStartup;

  //retry final qc event
  private String retryFinalQCEvent;

  //Product email event
  private String productEmailEvent;
}
