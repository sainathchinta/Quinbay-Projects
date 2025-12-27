package com.gdn.x.productcategorybase;

import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.context.annotation.Configuration;

import lombok.Data;

@Data
@Configuration
@ConfigurationProperties(value = "mandatory.parameter.default")
public class MandatoryParameterDefaultProperties {
  private String storeId;
  private String clientId;
  private String channelId;
  private String username;
}