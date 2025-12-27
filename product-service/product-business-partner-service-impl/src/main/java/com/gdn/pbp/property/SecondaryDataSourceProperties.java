package com.gdn.pbp.property;


import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.boot.autoconfigure.jdbc.DataSourceProperties;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.context.annotation.Configuration;

@Configuration
@ConditionalOnProperty(value = "spring.datasource.secondary.enabled", havingValue = "true")
@ConfigurationProperties(prefix = "spring.datasource.secondary")
public class
SecondaryDataSourceProperties extends DataSourceProperties {

}

