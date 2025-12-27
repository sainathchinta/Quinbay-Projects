package com.gdn.mta.bulk.config;

import lombok.Data;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.context.annotation.Configuration;

import java.util.Map;

@Data
@Configuration
@ConfigurationProperties(value = "ipr.products.source.display.name")
public class IprSourceDisplayNameProperties {
  public Map<String, String> map;
}
