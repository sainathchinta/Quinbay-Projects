package com.gdn.mta.product.config;

import org.springframework.beans.factory.config.PropertiesFactoryBean;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

import lombok.Data;

@Configuration
@Data
public class BeanConfiguration {

  @Bean
  public static CustomDeserializationProblemHandler customDeserializationProblemHandler() {
    return new CustomDeserializationProblemHandler();
  }

  @Bean
  public PropertiesFactoryBean sysparamProperties() {
    return new PropertiesFactoryBean();
  }


}
