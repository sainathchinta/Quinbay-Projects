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
public class CategoryAttributeWebResponse {

  private String id;
  private String name;
  private String attributeCode;
  private String attributeType;
  private boolean searchAble;
  private byte[] description;
  private boolean skuValue;
  private boolean isBasicView;
  private Integer sequence;
  private boolean isMainDefiningAttribute = false;
  private boolean isUSP = false;
  private boolean mainAttributeFlag;
  private String nameEnglish;
  private boolean screeningMandatory;
  private boolean variantCreatingUI;
  private byte[] descriptionEnglish;
  private boolean variantCreation;
  private boolean mandatory;
  private List<String> valueTypes = new ArrayList<>();
  private boolean sizeAttribute;
  private String attributeImageUrl;
  private boolean dsExtraction;
  private boolean multiLanguage;
  private boolean hideForSeller;
  private boolean hideForCustomer;
}
