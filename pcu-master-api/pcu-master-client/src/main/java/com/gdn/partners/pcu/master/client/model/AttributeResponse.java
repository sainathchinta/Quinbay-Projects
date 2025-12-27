package com.gdn.partners.pcu.master.client.model;

import java.util.ArrayList;
import java.util.List;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.x.productcategorybase.dto.BaseDTOResponse;

import com.gdn.x.productcategorybase.dto.response.PredefinedAllowedAttributeValueResponse;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.ToString;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@ToString
@JsonIgnoreProperties(ignoreUnknown = true)
public class AttributeResponse extends BaseDTOResponse {

  private static final long serialVersionUID = 1L;

  private String name;
  private String attributeCode;
  private String attributeType;
  private boolean searchAble;
  private byte[] description;
  private boolean skuValue;
  private List<AllowedAttributeValueResponse> allowedAttributeValues;
  private List<PredefinedAllowedAttributeValueResponse> predefinedAllowedAttributeValues;
  private boolean isBasicView;
  private String nameEnglish;
  private byte[] descriptionEnglish;
  private boolean screeningMandatory;
  private boolean variantCreatingUI;
  private boolean variantCreation;
  private boolean mandatory;
  private boolean extractedValue;
  private boolean sizeAttribute;
  private boolean valueTypeAttribute;
  private List<String> valueTypes = new ArrayList<>();
  private String attributeImageUrl;
  private boolean mustShowOnCustomerSide;
  private boolean dsExtraction;
  private boolean hideForSeller;
  private boolean hideForCustomer;
  private boolean multiLanguage;

  public AttributeResponse(String name, String attributeCode, String attributeType, boolean searchAble) {
    this(name, attributeCode, attributeType, searchAble, true);
  }

  public AttributeResponse(String name, String attributeCode, String attributeType, boolean searchAble,
      boolean isBasicView) {
    this.name = name;
    this.attributeCode = attributeCode;
    this.attributeType = attributeType;
    this.searchAble = searchAble;
    this.isBasicView = isBasicView;
  }
}
