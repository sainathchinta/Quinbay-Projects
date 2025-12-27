package com.gdn.x.product.domain.event.model;

import java.io.Serializable;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.GdnObjects;
import com.gdn.x.product.domain.event.enums.MasterDataAttributeType;

@JsonIgnoreProperties(ignoreUnknown = true)
public class MasterDataAttribute implements Serializable {

  private static final long serialVersionUID = -1747097301813885018L;

  public static final String DOCUMENT_NAME = "prd_master_data_attribute";

  private String attributeCode;

  private MasterDataAttributeType attributeType;

  private boolean mandatory;

  private String attributeName;

  private boolean isSearchable;

  private String description;

  private String example;

  private boolean isSkuValue;

  public MasterDataAttribute() {

  }

  public MasterDataAttribute(MasterDataAttributeType attributeType, boolean mandatory,
      String attributeName, boolean isSearchable, String description, String attributeCode,
      String example, boolean isSkuValue,
      List<MasterDataAllowedAttributeValue> masterDataAllowedAttributeValues,
      List<MasterDataPredefinedAllowedAttributeValue> masterDataPredefinedAllowedAttributeValues) {
    super();
    this.attributeType = attributeType;
    this.mandatory = mandatory;
    this.attributeName = attributeName;
    this.isSearchable = isSearchable;
    this.description = description;
    this.attributeCode = attributeCode;
    this.example = example;
    this.isSkuValue = isSkuValue;
  }

  @Override
  public boolean equals(Object anObject) {
    return GdnObjects.equals(this, anObject);
  }

  public String getAttributeCode() {
    return this.attributeCode;
  }

  public String getAttributeName() {
    return this.attributeName;
  }

  public MasterDataAttributeType getAttributeType() {
    return this.attributeType;
  }

  public String getDescription() {
    return this.description;
  }

  public String getExample() {
    return this.example;
  }

  @Override
  public int hashCode() {
    return GdnObjects.hashCode(this);
  }

  public boolean isMandatory() {
    return this.mandatory;
  }

  public boolean isSearchable() {
    return this.isSearchable;
  }

  public boolean isSkuValue() {
    return this.isSkuValue;
  }

  public void setAttributeCode(String attributeCode) {
    this.attributeCode = attributeCode;
  }

  public void setAttributeName(String attributeName) {
    this.attributeName = attributeName;
  }

  public void setAttributeType(MasterDataAttributeType attributeType) {
    this.attributeType = attributeType;
  }

  public void setDescription(String description) {
    this.description = description;
  }

  public void setExample(String example) {
    this.example = example;
  }

  public void setMandatory(boolean mandatory) {
    this.mandatory = mandatory;
  }

  public void setSearchable(boolean isSearchable) {
    this.isSearchable = isSearchable;
  }

  public void setSkuValue(boolean isSkuValue) {
    this.isSkuValue = isSkuValue;
  }

  @Override
  public String toString() {
    return String
        .format(
            "MasterDataAttribute [attributeType=%s, mandatory=%s, attributeName=%s, isSearchable=%s, description=%s, attributeCode=%s, example=%s, isSkuValue=%s, toString()=%s]",
            this.attributeType, this.mandatory, this.attributeName, this.isSearchable,
            this.description, this.attributeCode, this.example, this.isSkuValue, super.toString());
  }
}
