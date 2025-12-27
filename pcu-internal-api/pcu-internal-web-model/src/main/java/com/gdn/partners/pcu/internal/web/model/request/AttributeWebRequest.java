package com.gdn.partners.pcu.internal.web.model.request;

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
  private List<AllowedAttributeValueWebRequest> allowedAttributeValues;
  private List<PredefinedAllowedAttributeValueWebRequest> predefinedAllowedAttributeValues;
  private boolean screeningMandatory;
  private boolean variantCreatingUI;
  private boolean variantCreation;
  private boolean dsExtraction;
  private boolean hideForSeller;
  private boolean hideForCustomer;
  private boolean multiLanguage;
}