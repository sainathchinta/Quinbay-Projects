package com.gdn.mta.product.entity;

import java.io.Serializable;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.web.base.BaseResponse;
import lombok.ToString;

@ToString
@JsonIgnoreProperties(ignoreUnknown = true)
public class ProductLevel3Attribute extends BaseResponse implements Serializable {

  private static final long serialVersionUID = 6570782958685439755L;
  private String attributeCode;
  private String attributeType;
  private List<String> values;
  private String valueType;
  private Boolean skuValue;
  private String attributeName;
  private String itemSku;
  private boolean variantCreation;
  private boolean mandatory;
  private boolean basicView;
  
  public ProductLevel3Attribute() {
    // do nothing
  }

  public ProductLevel3Attribute(String attributeCode, String attributeType, List<String> values) {
    super();
    this.attributeCode = attributeCode;
    this.attributeType = attributeType;
    this.values = values;
  }

  public ProductLevel3Attribute(String attributeCode, String attributeType, List<String> values,
      Boolean skuValue) {
    this(attributeCode, attributeType, values);
    this.skuValue = skuValue;
  }

  public ProductLevel3Attribute(String attributeCode, String attributeType, List<String> values,
      Boolean skuValue, String attributeName) {
    this(attributeCode, attributeType, values, skuValue);
    this.attributeName = attributeName;
  }

  public ProductLevel3Attribute(String attributeCode, String attributeType, List<String> values,
      Boolean skuValue, String attributeName, String itemSku) {
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

  public void setVariantCreation(boolean variantCreation) {
    this.variantCreation = variantCreation;
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

  public String getValueType() {
    return valueType;
  }

  public void setValueType(String valueType) {
    this.valueType = valueType;
  }

}
