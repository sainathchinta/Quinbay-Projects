package com.gda.mta.product.dto;

import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.web.base.BaseResponse;

@JsonIgnoreProperties(ignoreUnknown = true)
public class ProductLevel3AttributeResponse extends BaseResponse {

  private static final long serialVersionUID = -2127700571462374275L;
  private String attributeCode;
  private String attributeType;
  private List<String> values;
  private String valueType;
  private boolean sizeAttribute;
  private Boolean skuValue;
  private String attributeName;
  private String itemSku;
  private boolean variantCreation;
  private boolean mandatory;
  private boolean basicView;
  private boolean extractedValue;
  private boolean dsExtraction;
  private boolean hideFromSeller;

  public ProductLevel3AttributeResponse() {
    // do nothing
  }
  
  public ProductLevel3AttributeResponse(String attributeCode, String attributeType,
      List<String> values) {
    super();
    this.attributeCode = attributeCode;
    this.attributeType = attributeType;
    this.values = values;
  }

  public ProductLevel3AttributeResponse(String attributeCode, String attributeType,
      List<String> values, Boolean skuValue) {
    this(attributeCode, attributeType, values);
    this.skuValue = skuValue;
  }

  public ProductLevel3AttributeResponse(String attributeCode, String attributeType,
      List<String> values, Boolean skuValue, String attributeName) {
    this(attributeCode, attributeType, values, skuValue);
    this.attributeName = attributeName;
  }

  public ProductLevel3AttributeResponse(String attributeCode, String attributeType,
      List<String> values, Boolean skuValue, String attributeName, String itemSku) {
    this(attributeCode, attributeType, values, skuValue, attributeName);
    this.itemSku = itemSku;
  }

  public String getAttributeCode() {
    return attributeCode;
  }

  public void setAttributeCode(String attributeCode) {
    this.attributeCode = attributeCode;
  }

  public String getAttributeType() {
    return attributeType;
  }

  public void setAttributeType(String attributeType) {
    this.attributeType = attributeType;
  }

  public String getValueType() {
    return valueType;
  }

  public void setValueType(String valueType) {
    this.valueType = valueType;
  }

  public boolean getSizeAttribute() {
    return sizeAttribute;
  }

  public void setSizeAttribute(boolean sizeAttribute) {
    this.sizeAttribute = sizeAttribute;
  }

  public List<String> getValues() {
    return values;
  }

  public void setValues(List<String> values) {
    this.values = values;
  }

  public Boolean getSkuValue() {
    return skuValue;
  }

  public void setSkuValue(Boolean skuValue) {
    this.skuValue = skuValue;
  }

  public String getAttributeName() {
    return attributeName;
  }

  public void setAttributeName(String attributeName) {
    this.attributeName = attributeName;
  }

  public String getItemSku() {
    return itemSku;
  }

  public void setItemSku(String itemSku) {
    this.itemSku = itemSku;
  }

  public boolean isVariantCreation() {
    return variantCreation;
  }

  public boolean isMandatory() {
    return mandatory;
  }

  public void setMandatory(boolean mandatory) {
    this.mandatory = mandatory;
  }

  public boolean isBasicView() {
    return basicView;
  }

  public void setBasicView(boolean basicView) {
    this.basicView = basicView;
  }

  public void setVariantCreation(boolean variantCreation) {
    this.variantCreation = variantCreation;
  }

  public boolean isExtractedValue() {
    return extractedValue;
  }

  public void setExtractedValue(boolean extractedValue) {
    this.extractedValue = extractedValue;
  }

  public boolean isDsExtraction() {
    return dsExtraction;
  }

  public void setDsExtraction(boolean dsExtraction) {
    this.dsExtraction = dsExtraction;
  }

  public boolean isHideFromSeller() {
    return hideFromSeller;
  }

  public void setHideFromSeller(boolean hideFromSeller) {
    this.hideFromSeller = hideFromSeller;
  }

  @Override
  public String toString() {
    return String
        .format(
            "ProductLevel3AttributeResponse [attributeCode=%s, attributeType=%s, values=%s, skuValue=%s, attributeName=%s, itemSku=%s, variantCreation=%s, basicView=%s, mandatory=%s,  getAttributeCode()=%s, getAttributeType()=%s, getValues()=%s, getSkuValue()=%s, getAttributeName()=%s, getItemSku()=%s, getVariantCreation=%s, getBasicView=%s, getMandatory=%s, extractedValue=%s, dsExtraction=%s, hideFromSeller=%s]",
            attributeCode, attributeType, values, skuValue, attributeName, itemSku, variantCreation, basicView, mandatory,extractedValue,
            getAttributeCode(), getAttributeType(), getValues(), getSkuValue(), getAttributeName(),
            getItemSku(), isVariantCreation(), isBasicView(), isMandatory(), isExtractedValue() , isDsExtraction() , isHideFromSeller());
  }

}
