package com.gdn.mta.bulk.property;

import java.util.List;

import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.context.annotation.Configuration;

import lombok.Data;

@Data
@Configuration
@ConfigurationProperties(prefix = "secondary.datasource")
public class SecondaryDataSourceApis {

  private List<String> apis;

}
