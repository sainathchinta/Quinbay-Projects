package com.gdn.partners.product.analytics.client.helper;

import java.util.UUID;

import org.apache.commons.lang3.StringUtils;
import org.slf4j.MDC;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Configuration;

import com.gdn.partners.product.analytics.client.constants.ClientParameter;
import com.gdn.partners.product.analytics.client.constants.ClientParameterConstants;
import com.gdn.partners.product.analytics.properties.ClientParameterProperties;
import lombok.Builder;

@Builder
@Configuration
public class ClientParameterHelper {

  @Autowired
  ClientParameterProperties clientParameterProperties;

  public ClientParameter get() {
    return ClientParameter.builder().storeId(getStoreId()).requestId(getRequestId()).channelId(getChannelId())
        .clientId(getClientId()).username(getUsername()).build();
  }

  public String getStoreId() {
    return getValueFromMDC(ClientParameterConstants.STORE_ID, clientParameterProperties.getStoreId());
  }

  public String getChannelId() {
    return getValueFromMDC(ClientParameterConstants.CHANNEL_ID, clientParameterProperties.getChannelId());
  }

  public String getClientId() {
    return getValueFromMDC(ClientParameterConstants.CLIENT_ID, clientParameterProperties.getClientId());
  }

  public String getRequestId() {
    String defaultRequestId = UUID.randomUUID().toString();
    return getValueFromMDC(ClientParameterConstants.REQUEST_ID, defaultRequestId);
  }


  public String getUsername() {
    return getValueFromMDC(ClientParameterConstants.USERNAME, clientParameterProperties.getUsername());
  }

  private String getValueFromMDC(String key, String defaultValue) {
    return StringUtils.defaultIfBlank(MDC.get(key), defaultValue);
  }

}
