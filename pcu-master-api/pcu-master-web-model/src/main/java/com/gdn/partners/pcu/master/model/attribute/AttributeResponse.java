package com.gdn.partners.pcu.master.model.attribute;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.ArrayList;
import java.util.List;

/**
 * Created by govind on 28/10/2018 AD.
 */

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@JsonInclude(JsonInclude.Include.ALWAYS)
@JsonIgnoreProperties(ignoreUnknown = true)
public class AttributeResponse {

  private String id;
  private String name;
  private String englishName;
  private String attributeCode;
  private String attributeType;
  private boolean searchAble;
  private byte[] description;
  private byte[] englishDescription;
  private boolean skuValue;
  private boolean isBasicView;
  private boolean variantCreation;
  private boolean mandatory;
  private boolean sizeAttribute;
  private boolean valueTypeAttribute;
  private List<String> valueTypes = new ArrayList<>();
  private String attributeImageUrl;
  private boolean dsExtraction;
  private boolean hideForSeller;
  private boolean hideForCustomer;
  private boolean multiLanguage;
}
