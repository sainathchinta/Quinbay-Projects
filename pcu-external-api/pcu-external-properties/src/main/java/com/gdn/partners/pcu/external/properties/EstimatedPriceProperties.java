package com.gdn.partners.pcu.external.properties;

import org.springframework.boot.context.properties.ConfigurationProperties;

import lombok.Data;

@Data
@ConfigurationProperties(value = "item")
public class EstimatedPriceProperties {

  private double lowestPriceCoefficient;

}
