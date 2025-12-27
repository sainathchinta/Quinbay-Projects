package com.gdn.partners.product.analytics.properties;

import lombok.Data;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.stereotype.Component;

import java.util.Map;

@Data
@Component
@ConfigurationProperties(value = "kafka.topic")
public class KafkaTopicProperties {
  private String deleteAutoApprovedProductEventName;
  private Map<String, String> permanentDeleteProductEventsMappedToService;
  private String permanentDeleteProductEventName;
  private String productImageDeleteEventName;
  private String iprProductsEventName;
  private String productOptimisationEventName;
  private String productChangeEventName;
  private String sellerCacheClearEventName;
  private String attributeChangeEventName;
  private String productAttributeExtractionsValidationEventName;
  private String productAttributeExtractionsEventName;
  private String productAttributeFeedbackEventName;
  private String agpPermanentDeleteProductEventName;
  // Auto start up
  private boolean autoStartup;
}
