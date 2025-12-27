package com.gdn.partners.pcu.internal.properties;

import org.springframework.boot.context.properties.ConfigurationProperties;

import lombok.Data;

@Data
@ConfigurationProperties(value = "ext.catalog")
public class ExtCatalogProperties {

  private String needPristineSuggestion;
  private int pristineTimeoutPeriod;
  private String pristineCategories;

}
