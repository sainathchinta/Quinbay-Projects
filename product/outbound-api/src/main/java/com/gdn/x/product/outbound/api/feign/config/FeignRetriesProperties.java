package com.gdn.x.product.outbound.api.feign.config;

import lombok.Data;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.stereotype.Component;

@Component
@ConfigurationProperties(prefix = "retry.feign")
@Data
public class FeignRetriesProperties {
  private int pricing;
  private int inventory;
  private int pcb;
  private int campaign;
  private int businessPartner;
  private int promotion;
  private int productAnalytics;
  private int wareHouse;
}
