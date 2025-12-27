package com.gda.mta.product.dto;

import java.util.List;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import lombok.ToString;

@ToString
@JsonIgnoreProperties(ignoreUnknown = true)
public class ProductLevel3AttributeRequest extends BaseRequest {

  private static final long serialVersionUID = -5024474113981110703L;
  private String attributeCode;
  private String attributeType;
  private List<String> values;
  private String valueType;
  private Boolean skuValue;
  private String attributeName;
  private String itemSku;
  private boolean mandatory;
  private boolean basicView;
  private boolean variantCreation;

  public ProductLevel3AttributeRequest() {}

  public ProductLevel3AttributeRequest(String attributeCode, String attributeType,
      List<String> values) {
    super();
    this.attributeCode = attributeCode;
    this.attributeType = attributeType;
    this.values = values;
  }

  public ProductLevel3AttributeRequest(String attributeCode, String attributeType,
      List<String> values, Boolean skuValue) {
    this(attributeCode, attributeType, values);
    this.skuValue = skuValue;
  }

  public ProductLevel3AttributeRequest(String attributeCode, String attributeType,
      List<String> values, Boolean skuValue, String attributeName) {
    this(attributeCode, attributeType, values, skuValue);
    this.attributeName = attributeName;
  }

  public ProductLevel3AttributeRequest(String attributeCode, String attributeType,
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

  public static long getSerialversionuid() {
    return serialVersionUID;
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

  public boolean isVariantCreation() {
    return variantCreation;
  }

  public void setVariantCreation(boolean variantCreation) {
    this.variantCreation = variantCreation;
  }

  public String getValueType() {
    return valueType;
  }

  public void setValueType(String valueType) {
    this.valueType = valueType;
  }
  
}
