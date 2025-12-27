package com.gdn.partners.pcu.master.properties;

import lombok.Data;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.stereotype.Component;

@Data
@ConfigurationProperties(value = "kafka.topic")
@Component
public class KafkaTopicProperties {

  private String deleteSizeChartEventName;
}