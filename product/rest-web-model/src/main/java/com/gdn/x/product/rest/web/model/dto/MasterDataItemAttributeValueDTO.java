package com.gdn.x.product.rest.web.model.dto;

import java.io.Serializable;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.GdnObjects;

@JsonIgnoreProperties(ignoreUnknown = true)
public class MasterDataItemAttributeValueDTO implements Serializable {

  private static final long serialVersionUID = 1L;

  private String attributeValue;
  private MasterDataAttributeDTO masterDataAttribute;
  private String valueType;

  public MasterDataItemAttributeValueDTO() {

  }

  public MasterDataItemAttributeValueDTO(String attributeValue,
      MasterDataAttributeDTO masterDataAttribute) {
    super();
    this.attributeValue = attributeValue;
    this.masterDataAttribute = masterDataAttribute;
  }

  @Override
  public boolean equals(Object obj) {
    return GdnObjects.equals(this, obj);
  }

  public String getAttributeValue() {
    return this.attributeValue;
  }

  public MasterDataAttributeDTO getMasterDataAttribute() {
    return this.masterDataAttribute;
  }

  @Override
  public int hashCode() {
    return GdnObjects.hashCode(this);
  }

  public void setAttributeValue(String attributeValue) {
    this.attributeValue = attributeValue;
  }

  public void setMasterDataAttribute(MasterDataAttributeDTO masterDataAttribute) {
    this.masterDataAttribute = masterDataAttribute;
  }


  public String getValueType() {
    return valueType;
  }

  public void setValueType(String valueType) {
    this.valueType = valueType;
  }

  @Override
  public String toString() {
    return String
        .format(
            "MasterDataItemAttributeValueDTO [attributeValue=%s, masterDataAttribute=%s, valueType=%s, toString()=%s]",
            this.attributeValue, this.masterDataAttribute, this.valueType, super.toString());
  }
}
