package com.gdn.mta.product.service.config;

import lombok.Data;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.stereotype.Component;

@Component
@ConfigurationProperties(prefix = "pre.order.config")
@Data
public class PreOrderConfig {
  private boolean poQuotaFeatureSwitch;
}
