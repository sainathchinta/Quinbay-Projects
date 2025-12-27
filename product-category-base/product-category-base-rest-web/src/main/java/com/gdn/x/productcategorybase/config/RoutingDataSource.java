package com.gdn.x.productcategorybase.config;

import java.util.Map;

import org.springframework.jdbc.datasource.lookup.AbstractRoutingDataSource;

import com.zaxxer.hikari.HikariDataSource;

public class RoutingDataSource extends AbstractRoutingDataSource {

  private MandatoryParameterHelper mandatoryParameterHelper;

  public RoutingDataSource(HikariDataSource primaryHikariDataSource, HikariDataSource secondaryHikariDataSource,
      MandatoryParameterHelper mandatoryParameterHelper) {
    this.setTargetDataSources(
        Map.of(DataSourceType.PRIMARY.name(), primaryHikariDataSource, DataSourceType.SECONDARY.name(),
            secondaryHikariDataSource));
    this.setDefaultTargetDataSource(primaryHikariDataSource);
    this.mandatoryParameterHelper = mandatoryParameterHelper;
  }

  @Override
  protected Object determineCurrentLookupKey() {
    return mandatoryParameterHelper.getDataSource();
  }
}
