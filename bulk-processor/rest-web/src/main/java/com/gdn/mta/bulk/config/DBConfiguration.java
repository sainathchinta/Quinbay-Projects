package com.gdn.mta.bulk.config;

import java.util.Optional;

import javax.sql.DataSource;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.boot.autoconfigure.condition.ConditionalOnBean;
import org.springframework.boot.autoconfigure.jdbc.DataSourceProperties;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Primary;

import com.gdn.mta.bulk.property.SecondaryDataSourceProperties;
import com.zaxxer.hikari.HikariDataSource;

@Configuration
public class DBConfiguration {

  @Autowired
  private MandatoryParameterHelper mandatoryParameterHelper;

  @Bean
  @Primary
  @Qualifier("primaryDataSourceProperties")
  public DataSourceProperties primaryDataSourceProperties() {
    return new DataSourceProperties();
  }

  @Bean
  @Qualifier("primaryDataSource")
  @ConfigurationProperties("spring.datasource")
  public HikariDataSource primaryDataSource(DataSourceProperties primaryDataSourceProperties) {
    return DataSourceConfiguration.buildDataSource(primaryDataSourceProperties);
  }

  @Bean
  @Qualifier("secondaryDataSource")
  @ConditionalOnBean(name = { "secondaryDataSourceProperties" })
  @ConfigurationProperties("spring.datasource.secondary")
  public HikariDataSource secondaryDataSource(SecondaryDataSourceProperties secondaryDataSourceProperties) {
    return DataSourceConfiguration.buildDataSource(secondaryDataSourceProperties);
  }

  @Bean
  @Primary
  public DataSource dataSource(HikariDataSource primaryDataSource, Optional<HikariDataSource> secondaryDataSource) {
    return secondaryDataSource.isPresent() ?
        new RoutingDataSource(primaryDataSource, secondaryDataSource.get(), mandatoryParameterHelper) : primaryDataSource;
  }
}
