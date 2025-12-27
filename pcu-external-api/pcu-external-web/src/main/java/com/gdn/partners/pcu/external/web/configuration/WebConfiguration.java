package com.gdn.partners.pcu.external.web.configuration;

import com.gdn.partners.core.security.interceptor.AuthorizationInterceptor;
import com.gdn.partners.pcu.external.model.Constants;
import com.gdn.partners.pcu.external.web.interceptor.SecurityInterceptor;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.http.MediaType;
import org.springframework.web.servlet.config.annotation.ContentNegotiationConfigurer;
import org.springframework.web.servlet.config.annotation.InterceptorRegistry;
import org.springframework.web.servlet.config.annotation.PathMatchConfigurer;
import org.springframework.web.servlet.config.annotation.WebMvcConfigurer;

/**
 * @author Pradeep Reddy
 */
@Configuration
public class WebConfiguration implements WebMvcConfigurer {

  @Bean
  public SecurityInterceptor securityInterceptor() {
    return new SecurityInterceptor();
  }

  @Override
  public void addInterceptors(InterceptorRegistry registry) {
    registry.addInterceptor(this.securityInterceptor()).addPathPatterns(Constants.CONTEXT_PATH + "/**");
    registry.addInterceptor(new AuthorizationInterceptor()).addPathPatterns(Constants.CONTEXT_PATH + "/**");
  }

  @Override
  public void configureContentNegotiation(ContentNegotiationConfigurer configure){
    configure.defaultContentType(MediaType.APPLICATION_JSON);
  }

  @Override
  public void configurePathMatch(PathMatchConfigurer configurer) {
    configurer.setUseTrailingSlashMatch(true);
  }

}
