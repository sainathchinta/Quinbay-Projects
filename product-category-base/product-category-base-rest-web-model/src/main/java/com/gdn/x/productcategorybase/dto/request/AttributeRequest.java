package com.gdn.x.productcategorybase.dto.request;

import java.util.ArrayList;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.x.productcategorybase.dto.AttributeType;
import com.gdn.x.productcategorybase.dto.BaseDTORequest;

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
public class AttributeRequest extends BaseDTORequest {

  private static final long serialVersionUID = -7651896486150281249L;

  private String name;
  private String attributeCode;
  private AttributeType attributeType;
  private boolean searchAble = false;
  private boolean mandatory;
  private byte[] description;
  private boolean skuValue = false;
  private String example;
  private boolean isBasicView = true;
  private boolean sizeAttribute = false;
  private boolean valueTypeAttribute = false;
  private List<String> valueTypes = new ArrayList<>();
  private String attributeImageUrl;
  private boolean screeningMandatory;
  private boolean variantCreatingUI;
  private boolean variantCreation;
  private boolean mustShowOnCustomerSide;
  private boolean dsExtraction;
  private boolean hideForSeller;
  private boolean hideForCustomer;
  private boolean multiLanguage;

  private List<AllowedAttributeValueRequest> allowedAttributeValues = new ArrayList<>();

  private List<PredefinedAllowedAttributeValueRequest> predefinedAllowedAttributeValues = new ArrayList<>();

  private List<DimensionMappingRequest> dimensionMapping = new ArrayList<>();

  public AttributeRequest(String name, String attributeCode, AttributeType attributeType, boolean searchAble,
      boolean mandatory, String storeId) {
    this(name, attributeCode, attributeType, searchAble, mandatory, true, storeId);
  }

  public AttributeRequest(String name, String attributeCode, AttributeType attributeType, boolean searchAble,
      boolean mandatory, boolean isBasicView, String storeId) {
    this.name = name;
    this.attributeCode = attributeCode;
    this.attributeType = attributeType;
    this.searchAble = searchAble;
    this.mandatory = mandatory;
    this.isBasicView = isBasicView;
    this.setStoreId(storeId);
  }
}
