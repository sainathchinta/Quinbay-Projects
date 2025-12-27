package com.gdn.partners.pcu.master.properties;

import org.springframework.boot.context.properties.ConfigurationProperties;

import lombok.Data;

/**
 * @author Pradeep Reddy
 */
@Data
@ConfigurationProperties(value = "client.parameter")
public class ClientParameterProperties {

  private String storeId;
  private String clientId;
  private String channelId;
  private String requestId;
  private String username;
  private String businessPartnerCode;

}
