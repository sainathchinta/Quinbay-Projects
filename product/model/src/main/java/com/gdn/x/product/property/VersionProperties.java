package com.gdn.x.product.property;

import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.context.annotation.Configuration;

import lombok.Data;

@Data
@Configuration
@ConfigurationProperties("application")
public class VersionProperties {

  private boolean production = true;
}
