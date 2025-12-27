package com.gdn.pbp.config;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Configuration;
import org.springframework.web.servlet.config.annotation.ContentNegotiationConfigurer;
import org.springframework.web.servlet.config.annotation.EnableWebMvc;
import org.springframework.web.servlet.config.annotation.InterceptorRegistry;
import org.springframework.web.servlet.config.annotation.PathMatchConfigurer;
import org.springframework.web.servlet.config.annotation.ViewControllerRegistry;
import org.springframework.web.servlet.config.annotation.WebMvcConfigurer;

import com.gdn.pbp.property.MandatoryParameterInterceptor;

@Configuration
@EnableWebMvc
public class WebConfiguration implements WebMvcConfigurer {

  @Autowired
  private MandatoryParameterInterceptor mandatoryParameterInterceptor;

  @Override
  public void configurePathMatch(PathMatchConfigurer configurer) {
    configurer.setUseTrailingSlashMatch(true);
  }

  @Override
  public void addInterceptors(InterceptorRegistry registry) {
    registry.addInterceptor(mandatoryParameterInterceptor).addPathPatterns("/**").excludePathPatterns("/docs/**")
        .excludePathPatterns("/v2/**").excludePathPatterns("/version").excludePathPatterns("/sys-info/healthcheck")
        .excludePathPatterns("/configuration/**").excludePathPatterns("/swagger-resources/**")
        .excludePathPatterns("/swagger-ui.html");
  }

  @Override
  public void addViewControllers(ViewControllerRegistry registry) {
    registry.addRedirectViewController("/docs/v2/api-docs", "/v2/api-docs");
    registry.addRedirectViewController("/docs/configuration/ui", "/configuration/ui");
    registry.addRedirectViewController("/docs/configuration/security", "/configuration/security");
    registry.addRedirectViewController("/docs/swagger-resources", "/swagger-resources");
    registry.addRedirectViewController("/docs", "/swagger-ui.html");
  }

  @Override
  public void configureContentNegotiation(ContentNegotiationConfigurer configurer) {
    configurer.favorParameter(false).ignoreAcceptHeader(true)
        .defaultContentType(org.springframework.http.MediaType.APPLICATION_JSON);
  }
}