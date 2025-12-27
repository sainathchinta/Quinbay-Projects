package com.gdn.x.mta.distributiontask.config;

import com.zaxxer.hikari.HikariDataSource;
import lombok.AccessLevel;
import lombok.NoArgsConstructor;
import org.apache.commons.lang3.StringUtils;
import org.springframework.boot.autoconfigure.jdbc.DataSourceProperties;

import java.util.Objects;

@NoArgsConstructor(access = AccessLevel.PRIVATE)

public class DataSourceConfiguration {
  public static HikariDataSource buildDataSource(DataSourceProperties dataSourceProperties,
    Boolean autoCommitValue) {
    HikariDataSource dataSource =
        dataSourceProperties.initializeDataSourceBuilder().type(HikariDataSource.class).build();
    if (StringUtils.isNotBlank(dataSourceProperties.getName())) {
      dataSource.setPoolName(dataSourceProperties.getName());
    }
    if (Objects.nonNull(autoCommitValue)) {
      dataSource.setAutoCommit(autoCommitValue);
    }
    return dataSource;
  }
}
