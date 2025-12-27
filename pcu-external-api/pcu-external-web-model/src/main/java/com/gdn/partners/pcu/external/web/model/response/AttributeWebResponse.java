package com.gdn.partners.pcu.external.web.model.response;

import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;


@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
@JsonInclude(JsonInclude.Include.ALWAYS)
@JsonIgnoreProperties(ignoreUnknown = true)
public class AttributeWebResponse {

  private String id;
  private String name;
  private String attributeCode;
  private String attributeType;
  private boolean searchAble;
  private byte[] description;
  private boolean skuValue;
  private List<AllowedAttributeValueWebResponse> allowedAttributeValues;
  private List<PredefinedAttributeValueWebResponse> predefinedAllowedAttributeValues;
  private boolean isBasicView;
  private String nameEnglish;
  private byte[] descriptionEnglish;
  private boolean variantCreation;
  private boolean mandatory;
}
