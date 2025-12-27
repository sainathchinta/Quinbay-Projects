package com.gdn.partners.pcu.internal.web.model.request;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class ProductAttributeValueWebRequest {

  private String id;
  private AllowedAttributeValueWebRequest allowedAttributeValue;
  private String descriptiveAttributeValue;
  private DescriptiveAttributeValueTypeWeb descriptiveAttributeValueType;
  private PredefinedAllowedAttributeValueWebRequest predefinedAllowedAttributeValue;

}