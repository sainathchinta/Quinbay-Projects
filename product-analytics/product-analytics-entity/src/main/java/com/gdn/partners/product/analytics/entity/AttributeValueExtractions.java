package com.gdn.partners.product.analytics.entity;

import lombok.Builder;
import lombok.Data;

@Data
@Builder
public class AttributeValueExtractions {

  private String attributeName;
  private String attributeValue;
  private String attributeValueEnglish;
}