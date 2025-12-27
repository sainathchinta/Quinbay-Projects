package com.gdn.pbp.property;

import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.context.annotation.Configuration;
import org.springframework.stereotype.Component;

import lombok.Data;

@Data
@Configuration
@Component
@ConfigurationProperties(value = "mandatory.parameter.default")
public class MandatoryParameterDefaultProperties {

  private String storeId;
  private String clientId;
  private String channelId;
  private String username;

}
