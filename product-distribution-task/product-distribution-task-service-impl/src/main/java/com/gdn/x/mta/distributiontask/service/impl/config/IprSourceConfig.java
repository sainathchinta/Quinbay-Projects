package com.gdn.x.mta.distributiontask.service.impl.config;

import lombok.Data;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.context.annotation.Configuration;

import java.util.Map;

@Data
@Configuration
@ConfigurationProperties(value = "ipr.source.name")
public class IprSourceConfig {
  public Map<String, String> sourceNameMap;
}
