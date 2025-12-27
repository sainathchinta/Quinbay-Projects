package com.gdn.x.product.outbound.api.feign.config;

import feign.Retryer;
import lombok.Data;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.stereotype.Component;

import java.util.concurrent.TimeUnit;

@Component
@ConfigurationProperties(prefix = "product.pricing")
@Data
public class PricingFeignProperties {
  private Integer connectTimeout;
  private Integer readTimeout;

  @Autowired
  private FeignRetriesProperties feignRetriesProperties;

  @Bean
  public Retryer getPricingRetryer() {
    return new Retryer.Default(100L, TimeUnit.SECONDS.toMillis(1L),
      feignRetriesProperties.getPricing());
  }
}
