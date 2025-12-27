package com.gdn.x.product.outbound.api.feign.config;

import java.util.concurrent.TimeUnit;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.stereotype.Component;

import feign.Retryer;
import lombok.Data;

@Component
@ConfigurationProperties(prefix = "pcb.feign")
@Data
public class PCBFeignProperties {
  private Integer connectTimeout;
  private Integer readTimeout;
  private String storeId;
  private String channelId;
  private String clientId;
  private String contextPath;
  private int port;

  @Autowired
  private FeignRetriesProperties feignRetriesProperties;

  @Bean
  public Retryer getPCBRetryer() {
    return new Retryer.Default(100L, TimeUnit.SECONDS.toMillis(1L),
      feignRetriesProperties.getPcb());
  }
}
