package com.gdn.partners.pcu.internal.properties;

import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.context.annotation.Configuration;

import lombok.Data;

/**
 * @author Pradeep Reddy
 */
@Data
@Configuration
@ConfigurationProperties(value = "client.parameter")
public class ClientParameterProperties {

  private String storeId;

  private String clientId;

  private String channelId;

  private String requestId;

  private String username;

  private String userType;

  private String businessPartnerCode;

  private String vendorCode;

}
