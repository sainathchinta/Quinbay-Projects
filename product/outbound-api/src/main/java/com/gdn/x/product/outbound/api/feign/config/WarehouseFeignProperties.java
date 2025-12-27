package com.gdn.x.product.outbound.api.feign.config;

import feign.Retryer;
import lombok.Data;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.stereotype.Component;

import java.util.concurrent.TimeUnit;

@Component
@ConfigurationProperties(prefix = "api.warehouse")
@Data
public class WarehouseFeignProperties {

  private Integer timeOut;
  private String port;

  @Autowired
  private FeignRetriesProperties feignRetriesProperties;

  @Bean
  public Retryer getWareHouseRetryer() {
    return new Retryer.Default(100L, TimeUnit.SECONDS.toMillis(1L),
      feignRetriesProperties.getWareHouse());
  }

}
