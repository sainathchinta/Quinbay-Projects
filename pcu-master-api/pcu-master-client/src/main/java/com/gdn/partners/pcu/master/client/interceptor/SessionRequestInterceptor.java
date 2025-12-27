package com.gdn.partners.pcu.master.client.interceptor;

import org.springframework.stereotype.Component;

import feign.RequestInterceptor;
import feign.RequestTemplate;

/**
 * @author Pradeep Reddy
 */
@Component
public class SessionRequestInterceptor implements RequestInterceptor {

  @Override
  public void apply(RequestTemplate template) {

  }
}
