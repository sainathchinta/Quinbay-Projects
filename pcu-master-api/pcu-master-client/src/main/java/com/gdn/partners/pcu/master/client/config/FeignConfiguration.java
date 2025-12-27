package com.gdn.partners.pcu.master.client.config;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

import com.gdn.partners.pcu.master.client.helper.ClientParameterHelper;
import com.gdn.partners.pcu.master.model.Constants;
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
  private ClientParameterHelper clientParameterHelper;

  @Bean
  public SetterFactory setterFactory() {
    return (target, method) -> HystrixCommand.Setter
        .withGroupKey(HystrixCommandGroupKey.Factory.asKey(target.name()))
        .andCommandKey(HystrixCommandKey.Factory.asKey(target.name()));
  }

  @Bean
  public RequestInterceptor requestInterceptor() {
    return requestTemplate -> {
      if (!(requestTemplate.headers().containsKey(Constants.USER_NAME))) {
        requestTemplate.header(Constants.USER_NAME, clientParameterHelper.getUsername());
      }
      if (!(requestTemplate.headers().containsKey(Constants.CHANNEL_ID))) {
        requestTemplate.header(Constants.CHANNEL_ID, clientParameterHelper.getChannelId());
      }
      if (!(requestTemplate.headers().containsKey(Constants.CLIENT_ID))) {
        requestTemplate.header(Constants.CLIENT_ID, clientParameterHelper.getClientId());
      }
      if (!(requestTemplate.headers().containsKey(Constants.REQUEST_ID))) {
        requestTemplate.header(Constants.REQUEST_ID, clientParameterHelper.getRequestId());
      }
      if (!(requestTemplate.queries().containsKey(Constants.STORE_ID))) {
        requestTemplate.query(Constants.STORE_ID, clientParameterHelper.getStoreId());
      }
      if (!(requestTemplate.queries().containsKey(Constants.REQUEST_ID))) {
        requestTemplate.query(Constants.REQUEST_ID, clientParameterHelper.getRequestId());
      }
      if (!(requestTemplate.queries().containsKey(Constants.CHANNEL_ID))) {
        requestTemplate.query(Constants.CHANNEL_ID, clientParameterHelper.getChannelId());
      }
      if (!(requestTemplate.queries().containsKey(Constants.CLIENT_ID))) {
        requestTemplate.query(Constants.CLIENT_ID, clientParameterHelper.getClientId());
      }
      if (!(requestTemplate.queries().containsKey(Constants.USER_NAME))) {
        requestTemplate.query(Constants.USER_NAME, clientParameterHelper.getUsername());
      }
    };
  }

}
