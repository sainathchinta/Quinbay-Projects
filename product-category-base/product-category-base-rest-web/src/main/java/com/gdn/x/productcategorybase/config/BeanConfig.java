package com.gdn.x.productcategorybase.config;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

import com.gdn.x.productcategorybase.MandatoryParameterDefaultProperties;

import brave.Tracer;

@Configuration
public class BeanConfig {

  @Autowired
  private Tracer tracer;

  @Autowired
  private TracerHelperImpl tracerHelper;

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
    return new MandatoryParameterHelper("system", tracer, tracerHelper, mandatoryParameterDefaultProperties);
  }

}
