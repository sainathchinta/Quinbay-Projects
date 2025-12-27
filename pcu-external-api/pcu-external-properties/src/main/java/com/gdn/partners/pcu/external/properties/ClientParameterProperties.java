package com.gdn.partners.pcu.external.properties;

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

}
