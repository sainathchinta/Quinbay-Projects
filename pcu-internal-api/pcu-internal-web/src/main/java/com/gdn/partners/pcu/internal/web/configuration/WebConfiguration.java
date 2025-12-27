package com.gdn.partners.pcu.internal.web.configuration;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.web.servlet.config.annotation.InterceptorRegistry;

import com.gdn.partners.core.security.interceptor.AuthorizationInterceptor;
import com.gdn.partners.pcu.internal.model.Constants;
import com.gdn.partners.pcu.internal.web.interceptor.SecurityInterceptor;
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
}
