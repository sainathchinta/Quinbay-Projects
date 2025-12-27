package com.gdn.partners.pcu.external.web.model.request;

import java.util.List;

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
public class AttributeWebRequest {
  private String id;
  private String name;
  private String attributeCode;
  private AttributeTypeWeb attributeType;
  private boolean searchAble;
  private boolean mandatory;
  private byte[] description;
  private boolean skuValue;
  private String example;
  private boolean isBasicView;
  private boolean variantCreation;
  private List<AllowedAttributeValueWebRequest> allowedAttributeValues;
  private List<PredefinedAllowedAttributeValueWebRequest> predefinedAllowedAttributeValues;
}
