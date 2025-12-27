package com.gdn.x.mta.distributiontask.config;

import com.zaxxer.hikari.HikariDataSource;
import org.springframework.jdbc.datasource.lookup.AbstractRoutingDataSource;

import java.util.Map;

public class RoutingDataSource extends AbstractRoutingDataSource {
  private MandatoryParameterHelper mandatoryParameterHelper;

  public RoutingDataSource(HikariDataSource primaryHikariDataSource,
      HikariDataSource secondaryHikariDataSource, MandatoryParameterHelper mandatoryParameterHelper) {
    this.setTargetDataSources(Map.of(DataSourceType.PRIMARY.name(), primaryHikariDataSource,
        DataSourceType.SECONDARY.name(), secondaryHikariDataSource));
    this.setDefaultTargetDataSource(primaryHikariDataSource);
    this.mandatoryParameterHelper = mandatoryParameterHelper;
  }

  @Override
  protected Object determineCurrentLookupKey() {
    return mandatoryParameterHelper.getDataSource();
  }

}