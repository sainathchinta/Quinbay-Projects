package com.gdn.x.mta.distributiontask.inbound.config;

import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.stereotype.Component;

import lombok.Data;

@Data
@ConfigurationProperties(value = "configuration.kafka.topic.consumer")
@Component
public class KafkaTopicPropertiesConsumer {
  // update to vendor solr event
  private String pdtProductCombinedUpdateToSolrEvent;

  // vendor approval events
  private String vendorCombinedEventNoPriority;
  private String vendorCombinedEventPriority1;
  private String vendorCombinedEventPriority2;

  //permanent delete event
  private String productAnalyticsPermanentDeleteEvent;

  // product change event from x-product
  private String productChangeEvent;
  private boolean autoStartup;

  //ipr creation event
  private String addCustomerProductToIprEvent;
  private String addIprProductSolrEvent;
  private String publishHistoryForIprEvent;
  private String suspendIprProductEvent;
  private String addProductMailEvent;
  private String addDSModelProductToIprEvent;


  //Product & Item Image Delete Event
  private String deleteOriginalImageEvent;

  // system parameter caffeine cache evict event
  private String systemParameterCaffeineCacheEvictEvent;

  // Product delete event
  private String deleteProductEvent;
}
