package com.gdn.x.mta.distributiontask.config;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

import brave.Tracer;

@Configuration
public class BeanConfig {

  @Autowired
  private Tracer tracer;

  @Autowired
  private TracerHelper tracerHelper;

  @Autowired
  private MandatoryParameterDefaultProperties mandatoryParameterDefaultProperties;

  @Autowired
  private SecondaryDataSourceApis secondaryDataSourceApis;

  @Bean
  public MandatoryParameterInterceptor mandatoryParameterInterceptor() {
    return new MandatoryParameterInterceptor(mandatoryParameterHelper(), secondaryDataSourceApis);
  }

  @Bean
  public MandatoryParameterHelper mandatoryParameterHelper() {
    return new MandatoryParameterHelper(tracer, tracerHelper, mandatoryParameterDefaultProperties);
  }
}
