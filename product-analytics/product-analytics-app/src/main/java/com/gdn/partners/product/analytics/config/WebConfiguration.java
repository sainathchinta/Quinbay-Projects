package com.gdn.partners.product.analytics.config;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.http.MediaType;
import org.springframework.web.servlet.config.annotation.ContentNegotiationConfigurer;
import org.springframework.web.servlet.config.annotation.EnableWebMvc;
import org.springframework.web.servlet.config.annotation.InterceptorRegistry;
import org.springframework.web.servlet.config.annotation.PathMatchConfigurer;
import org.springframework.web.servlet.config.annotation.ViewControllerRegistry;
import org.springframework.web.servlet.config.annotation.WebMvcConfigurer;

import com.gdn.x.base.WebApplicationPropertyHolder;

@Configuration
@EnableWebMvc
public class WebConfiguration implements WebMvcConfigurer {

  @Autowired
  private MandatoryParameterInterceptor mandatoryParameterInterceptor;

  @Value("${application.base.path.location}")
  private String applicationBasePathLocation;

  @Override
  public void addInterceptors(InterceptorRegistry registry) {
    registry.addInterceptor(mandatoryParameterInterceptor).addPathPatterns("/**").excludePathPatterns("/docs/**")
        .excludePathPatterns("/v2/**").excludePathPatterns("/version").excludePathPatterns("/sys-info/healthcheck")
        .excludePathPatterns("/configuration/**").excludePathPatterns("/swagger-resources/**")
        .excludePathPatterns("/swagger-ui.html");
  }
}