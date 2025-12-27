package com.gdn.partners.pcu.external.web.model.request;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * Created by govind on 20/12/2018 AD.
 */
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
