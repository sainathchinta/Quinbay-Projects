package com.gdn.x.product.service.config;

import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.stereotype.Component;

import lombok.Data;

@Component
@Data
@ConfigurationProperties(value = "configuration.data.source")
public class DataSourcePrimaryEnabledProperties {
  private boolean updateItemPickupPointReadFromPrimaryEnabled;
}
