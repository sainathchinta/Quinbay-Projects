package com.gdn.x.product.rest.web.properties;

import java.util.List;

import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.context.annotation.Configuration;

import lombok.Data;

@Data
@Configuration
@ConfigurationProperties(prefix = "primary.datasource")
public class PrimaryDataSourceApis {
  private List<String> apis;
  private List<String> events;
}
