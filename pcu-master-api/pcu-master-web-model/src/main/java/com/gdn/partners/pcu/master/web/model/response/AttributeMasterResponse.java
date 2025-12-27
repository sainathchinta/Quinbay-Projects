package com.gdn.partners.pcu.master.web.model.response;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.ArrayList;
import java.util.List;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
@JsonInclude(JsonInclude.Include.ALWAYS)
@JsonIgnoreProperties(ignoreUnknown = true)
public class AttributeMasterResponse {
  private String id;
  private String name;
  private String nameEnglish;
  private String dsAttributeName;
  private String attributeCode;
  private String attributeType;
  private byte[] description;
  private byte[] descriptionEnglish;
  private String example;
  private boolean searchAble;
  private boolean skuValue;
  private boolean isBasicView;
  private AttributeSortType sortType;
  private boolean variantCreation;
  private boolean mandatory;
  private boolean mustShowOnCustomerSide;
  private boolean valueTypeAttribute;
  private boolean sizeAttribute;
  private List<String> valueTypes = new ArrayList<>();
  private String attributeImageUrl;
  private boolean dsExtraction;
  private boolean hideForSeller;
  private boolean hideForCustomer;
  private boolean multiLanguage;
}
