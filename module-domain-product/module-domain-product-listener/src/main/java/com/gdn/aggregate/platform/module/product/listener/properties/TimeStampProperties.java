package com.gdn.aggregate.platform.module.product.listener.properties;

import java.util.HashMap;
import java.util.Map;
import java.util.Optional;

import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.context.annotation.Configuration;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Configuration
@ConfigurationProperties("enable.time.stamp")
public class TimeStampProperties {
  private Map<String, Boolean> timeStampConfigMap = new HashMap<>();

  public boolean considerTimeStamp(String topic, boolean defaultValue) {
    return Optional.ofNullable(this.timeStampConfigMap.getOrDefault(topic, defaultValue)).orElse(defaultValue);
  }
}
