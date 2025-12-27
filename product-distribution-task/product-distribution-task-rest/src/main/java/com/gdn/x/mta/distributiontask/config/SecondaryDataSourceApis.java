package com.gdn.x.mta.distributiontask.config;

import lombok.Data;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.context.annotation.Configuration;

import java.util.List;

@Data
@Configuration
@ConfigurationProperties(prefix = "secondary.datasource")
public class SecondaryDataSourceApis {
  private List<String> apis;

  public void setApis(String apis) {
    this.apis = List.of(apis.split(","));
  }
}