package com.gdn.partners.product.analytics.client.config;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

import com.gdn.partners.product.analytics.client.constants.ClientParameterConstants;
import com.gdn.partners.product.analytics.client.helper.ClientParameterHelper;
import com.netflix.hystrix.HystrixCommand;
import com.netflix.hystrix.HystrixCommandGroupKey;
import com.netflix.hystrix.HystrixCommandKey;
import feign.RequestInterceptor;
import feign.hystrix.SetterFactory;

/**
 * @author Pradeep Reddy
 */
@Configuration
public class FeignConfiguration {

  @Autowired
  ClientParameterHelper clientParameterHelper;


  @Bean
  public SetterFactory setterFactory() {
    return (target, method) -> HystrixCommand.Setter
        .withGroupKey(HystrixCommandGroupKey.Factory.asKey(target.name()))
        .andCommandKey(HystrixCommandKey.Factory.asKey(target.name()));
  }

  @Bean
  public RequestInterceptor requestInterceptor() {
    return requestTemplate -> {

      if (!(requestTemplate.headers().containsKey(ClientParameterConstants.USERNAME))) {
        requestTemplate.header(ClientParameterConstants.USERNAME, clientParameterHelper.getUsername());
      }

      if (!(requestTemplate.headers().containsKey(ClientParameterConstants.CHANNEL_ID))) {
        requestTemplate.header(ClientParameterConstants.CHANNEL_ID, clientParameterHelper.getChannelId());
      }

      if (!(requestTemplate.headers().containsKey(ClientParameterConstants.CLIENT_ID))) {
        requestTemplate.header(ClientParameterConstants.CLIENT_ID, clientParameterHelper.getClientId());
      }

      if (!(requestTemplate.headers().containsKey(ClientParameterConstants.REQUEST_ID))) {
        requestTemplate.header(ClientParameterConstants.REQUEST_ID, clientParameterHelper.getRequestId());
      }

      if (!(requestTemplate.queries().containsKey(ClientParameterConstants.STORE_ID))) {
        requestTemplate.query(ClientParameterConstants.STORE_ID, clientParameterHelper.getStoreId());
      }

      if (!(requestTemplate.queries().containsKey(ClientParameterConstants.REQUEST_ID))) {
        requestTemplate.query(ClientParameterConstants.REQUEST_ID, clientParameterHelper.getRequestId());
      }

      if (!(requestTemplate.queries().containsKey(ClientParameterConstants.CHANNEL_ID))) {
        requestTemplate.query(ClientParameterConstants.CHANNEL_ID, clientParameterHelper.getChannelId());
      }

      if (!(requestTemplate.queries().containsKey(ClientParameterConstants.CLIENT_ID))) {
        requestTemplate.query(ClientParameterConstants.CLIENT_ID, clientParameterHelper.getClientId());
      }

      if (!(requestTemplate.queries().containsKey(ClientParameterConstants.USERNAME))) {
        requestTemplate.query(ClientParameterConstants.USERNAME, clientParameterHelper.getUsername());
      }

    };
  }

}
