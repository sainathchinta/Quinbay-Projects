package com.gdn.partners.pcu.external.client.config;

import com.gdn.partners.pcu.external.client.helper.MandatoryParameterHelper;
import com.gdn.partners.pcu.external.model.Constants;
import com.netflix.hystrix.HystrixCommand;
import com.netflix.hystrix.HystrixCommandGroupKey;
import com.netflix.hystrix.HystrixCommandKey;
import feign.RequestInterceptor;
import feign.hystrix.SetterFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

/**
 * @author Pradeep Reddy
 */
@Configuration
public class FeignConfiguration {

  @Value("${default.agp.url}")
  private String agpUrl;

  @Autowired
  private MandatoryParameterHelper mandatoryParameterHelper;

  @Bean
  public SetterFactory setterFactory() {
    return (target, method) -> HystrixCommand.Setter
        .withGroupKey(HystrixCommandGroupKey.Factory.asKey(target.name()))
        .andCommandKey(HystrixCommandKey.Factory.asKey(target.name()));
  }

  @Bean
  public RequestInterceptor requestInterceptor() {
    return requestTemplate -> {

      if(requestTemplate.url().contains(agpUrl)) {
        return;
      }

      if (!(requestTemplate.headers().containsKey(Constants.STORE_ID))) {
        requestTemplate.header(Constants.STORE_ID, mandatoryParameterHelper.getStoreId());
      }
      if (!(requestTemplate.headers().containsKey(Constants.USER_NAME))) {
        requestTemplate.header(Constants.USER_NAME, mandatoryParameterHelper.getUsername());
      }
      if (!(requestTemplate.headers().containsKey(Constants.CHANNEL_ID))) {
        requestTemplate.header(Constants.CHANNEL_ID, mandatoryParameterHelper.getChannelId());
      }
      if (!(requestTemplate.headers().containsKey(Constants.CLIENT_ID))) {
        requestTemplate.header(Constants.CLIENT_ID, mandatoryParameterHelper.getClientId());
      }
      if (!(requestTemplate.headers().containsKey(Constants.REQUEST_ID))) {
        requestTemplate.header(Constants.REQUEST_ID, mandatoryParameterHelper.getRequestId());
      }

      if (!(requestTemplate.queries().containsKey(Constants.STORE_ID))) {
        requestTemplate.query(Constants.STORE_ID, mandatoryParameterHelper.getStoreId());
      }
      if (!(requestTemplate.queries().containsKey(Constants.REQUEST_ID))) {
        requestTemplate.query(Constants.REQUEST_ID, mandatoryParameterHelper.getRequestId());
      }
      if (!(requestTemplate.queries().containsKey(Constants.CHANNEL_ID))) {
        requestTemplate.query(Constants.CHANNEL_ID, mandatoryParameterHelper.getChannelId());
      }
      if (!(requestTemplate.queries().containsKey(Constants.CLIENT_ID))) {
        requestTemplate.query(Constants.CLIENT_ID, mandatoryParameterHelper.getClientId());
      }
      if (!(requestTemplate.queries().containsKey(Constants.USER_NAME))) {
        requestTemplate.query(Constants.USER_NAME, mandatoryParameterHelper.getUsername());
      }
    };
  }

}
