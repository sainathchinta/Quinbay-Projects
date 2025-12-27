package com.gdn.x.mta.distributiontask.config;

import com.zaxxer.hikari.HikariDataSource;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.autoconfigure.condition.ConditionalOnBean;
import org.springframework.boot.autoconfigure.jdbc.DataSourceProperties;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Primary;

import javax.sql.DataSource;
import java.util.Optional;

@Configuration
public class DBConfiguration {

  @Autowired
  private MandatoryParameterHelper mandatoryParameterHelper;

  @Value("${auto.commit.value}")
  private Boolean autoCommitValue;

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
    return DataSourceConfiguration.buildDataSource(primaryDataSourceProperties, autoCommitValue);
  }

  @Bean
  @Qualifier("secondaryDataSource")
  @ConditionalOnBean(name = {"secondaryDataSourceProperties"})
  public HikariDataSource secondaryDataSource(SecondaryDataSourceProperties secondaryDataSourceProperties) {
    return DataSourceConfiguration.buildDataSource(secondaryDataSourceProperties, autoCommitValue);
  }

  @Bean
  @Primary
  public DataSource dataSource(HikariDataSource primaryDataSource, Optional<HikariDataSource> secondaryDataSource) {
    return secondaryDataSource.isPresent() ?
        new RoutingDataSource(primaryDataSource, secondaryDataSource.get(), mandatoryParameterHelper) :
        primaryDataSource;
  }

}
