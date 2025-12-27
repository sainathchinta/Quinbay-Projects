package com.gdn.partners.pcu.internal.web.configuration;

import org.springframework.boot.web.servlet.FilterRegistrationBean;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;


/**
 * @author Pradeep Reddy
 */
@Configuration
public class FilterConfiguration {

  @Bean
  public FilterRegistrationBean credentialFilter() {
    CredentialFilter credentialFilter = new CredentialFilter();
    FilterRegistrationBean filterRegistrationBean = new FilterRegistrationBean(credentialFilter);
    filterRegistrationBean.setOrder(1);
    return filterRegistrationBean;
  }

}
