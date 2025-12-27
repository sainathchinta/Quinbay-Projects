package com.gdn.partners.pcu.master.properties;

import org.springframework.boot.context.properties.ConfigurationProperties;

import lombok.Data;

/**
 * @author Pradeep Reddy
 */
@Data
@ConfigurationProperties("application")
public class ApplicationProperties {

  private boolean production = true;

}
