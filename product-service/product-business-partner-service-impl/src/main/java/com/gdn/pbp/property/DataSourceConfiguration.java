package com.gdn.pbp.property;

import org.apache.commons.lang3.StringUtils;
import org.springframework.boot.autoconfigure.jdbc.DataSourceProperties;

import com.zaxxer.hikari.HikariDataSource;

import lombok.AccessLevel;
import lombok.NoArgsConstructor;

@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class DataSourceConfiguration {
  public static HikariDataSource buildDataSource(DataSourceProperties properties) {
    HikariDataSource dataSource = properties.initializeDataSourceBuilder().type(HikariDataSource.class).build();
    if (StringUtils.isNotBlank(properties.getName())) {
      dataSource.setPoolName(properties.getName());
    }
    return dataSource;
  }
}

