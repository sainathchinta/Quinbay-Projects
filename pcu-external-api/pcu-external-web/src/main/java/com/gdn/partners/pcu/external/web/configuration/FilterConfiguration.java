//package com.gdn.partners.pcu.external.web.configuration;
//
//import org.springframework.boot.web.servlet.FilterRegistrationBean;
//import org.springframework.context.annotation.Bean;
//import org.springframework.context.annotation.Configuration;
//
//import com.gdn.partners.core.filter.CredentialFilter;
//
///**
// * @author Pradeep Reddy
// */
//@Configuration
//public class FilterConfiguration {
//
//  @Bean
//  public FilterRegistrationBean credentialFilter() {
//    CredentialFilter credentialFilter = new CredentialFilter();
//    FilterRegistrationBean filterRegistrationBean = new FilterRegistrationBean(credentialFilter);
//    filterRegistrationBean.setOrder(1);
//    return filterRegistrationBean;
//  }
//
//}
