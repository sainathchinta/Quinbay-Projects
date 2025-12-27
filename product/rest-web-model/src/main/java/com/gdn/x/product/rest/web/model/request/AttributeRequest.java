package com.gdn.x.product.rest.web.model.request;

import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.web.base.BaseRequest;

@JsonIgnoreProperties(ignoreUnknown = true)
public class AttributeRequest extends BaseRequest {
  private static final long serialVersionUID = -8923376152988375813L;
  
  private String name;
  private String attributeCode;
  private String attributeType;
  private boolean searchAble;
  private byte[] description;
  private boolean skuValue;
  private List<AllowedAttributeValueRequest> allowedAttributeValues;
  private List<PredefinedAllowedAttributeValueRequest> predefinedAllowedAttributeValues;
  private String storeId;

  public String getName() {
    return name;
  }

  public void setName(String name) {
    this.name = name;
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

  public boolean isSearchAble() {
    return searchAble;
  }

  public void setSearchAble(boolean searchAble) {
    this.searchAble = searchAble;
  }

  public byte[] getDescription() {
    return description;
  }

  public void setDescription(byte[] description) {
    this.description = description;
  }

  public boolean isSkuValue() {
    return skuValue;
  }

  public void setSkuValue(boolean skuValue) {
    this.skuValue = skuValue;
  }

  public List<AllowedAttributeValueRequest> getAllowedAttributeValues() {
    return allowedAttributeValues;
  }

  public void setAllowedAttributeValues(List<AllowedAttributeValueRequest> allowedAttributeValues) {
    this.allowedAttributeValues = allowedAttributeValues;
  }

  public List<PredefinedAllowedAttributeValueRequest> getPredefinedAllowedAttributeValues() {
    return predefinedAllowedAttributeValues;
  }

  public void setPredefinedAllowedAttributeValues(
      List<PredefinedAllowedAttributeValueRequest> predefinedAllowedAttributeValues) {
    this.predefinedAllowedAttributeValues = predefinedAllowedAttributeValues;
  }

  public String getStoreId() {
    return storeId;
  }

  public void setStoreId(String storeId) {
    this.storeId = storeId;
  }
}
