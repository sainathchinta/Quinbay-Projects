package com.gdn.x.product.model.entity;

import jakarta.persistence.EnumType;
import jakarta.persistence.Enumerated;
import org.springframework.data.mongodb.core.mapping.Field;

import com.gdn.common.base.GdnObjects;
import com.gdn.x.product.enums.DescriptiveAttributeValueType;
import com.gdn.x.product.enums.ProductFieldNames;

public class MasterDataProductAttributeValue implements GdnBaseEmbedded {

  private static final long serialVersionUID = 1L;

  @Field(value = ProductFieldNames.DESCRIPTIVE_ATTRIBUTE_VALUE)
  private String descriptiveAttributeValue;

  @Enumerated(EnumType.STRING)
  @Field(value = ProductFieldNames.DESCRIPTIVE_ATTRIBUTE_VALUE_TYPE)
  private DescriptiveAttributeValueType descriptiveAttributeValueType;

  @Field(value = ProductFieldNames.ALLOWED_ATTRIBUTE_VALUE_CODE)
  private String allowedAttributeValueCode;

  @Field(value = ProductFieldNames.ALLOWED_ATTRIBUTE_VALUE)
  private MasterDataAllowedAttributeValue allowedAttributeValue;

  @Field(value = ProductFieldNames.PREDEFINED_ALLOWED_ATTRIBUTE_VALUE_CODE)
  private String predefinedAllowedAttributeValueCode;

  @Field(value = ProductFieldNames.PREDEFINED_ALLOWED_ATTRIBUTE_VALUE)
  private PredefinedAllowedAttributeValue predefinedAllowedAttributeValue;

  @Field(value = ProductFieldNames.MARK_FOR_DELETE)
  private boolean markForDelete;

  public MasterDataProductAttributeValue() {

  }

  public MasterDataProductAttributeValue(String descriptiveAttributeValue,
      DescriptiveAttributeValueType descriptiveAttributeValueType,
      String allowedAttributeValueCode, String predefinedAllowedAttributeValueCode,
      PredefinedAllowedAttributeValue predefinedAllowedAttributeValue, boolean markForDelete) {
    this.descriptiveAttributeValue = descriptiveAttributeValue;
    this.descriptiveAttributeValueType = descriptiveAttributeValueType;
    this.allowedAttributeValueCode = allowedAttributeValueCode;
    this.predefinedAllowedAttributeValueCode = predefinedAllowedAttributeValueCode;
    this.predefinedAllowedAttributeValue = predefinedAllowedAttributeValue;
    this.markForDelete = markForDelete;
  }

  @Override
  public boolean equals(Object obj) {
    return GdnObjects.equals(this, obj);
  }

  public MasterDataAllowedAttributeValue getAllowedAttributeValue() {
    return this.allowedAttributeValue;
  }

  public String getAllowedAttributeValueCode() {
    return this.allowedAttributeValueCode;
  }

  public String getDescriptiveAttributeValue() {
    return this.descriptiveAttributeValue;
  }

  public DescriptiveAttributeValueType getDescriptiveAttributeValueType() {
    return this.descriptiveAttributeValueType;
  }

  public PredefinedAllowedAttributeValue getPredefinedAllowedAttributeValue() {
    return this.predefinedAllowedAttributeValue;
  }

  public String getPredefinedAllowedAttributeValueCode() {
    return this.predefinedAllowedAttributeValueCode;
  }

  @Override
  public int hashCode() {
    return GdnObjects.hashCode(this);
  }

  public boolean isMarkForDelete() {
    return this.markForDelete;
  }

  public void setAllowedAttributeValue(MasterDataAllowedAttributeValue allowedAttributeValue) {
    this.allowedAttributeValue = allowedAttributeValue;
  }

  public void setAllowedAttributeValueCode(String allowedAttributeValueCode) {
    this.allowedAttributeValueCode = allowedAttributeValueCode;
  }

  public void setDescriptiveAttributeValue(String descriptiveAttributeValue) {
    this.descriptiveAttributeValue = descriptiveAttributeValue;
  }

  public void setDescriptiveAttributeValueType(
      DescriptiveAttributeValueType descriptiveAttributeValueType) {
    this.descriptiveAttributeValueType = descriptiveAttributeValueType;
  }

  public void setMarkForDelete(boolean markForDelete) {
    this.markForDelete = markForDelete;
  }

  public void setPredefinedAllowedAttributeValue(
      PredefinedAllowedAttributeValue predefinedAllowedAttributeValue) {
    this.predefinedAllowedAttributeValue = predefinedAllowedAttributeValue;
  }

  public void setPredefinedAllowedAttributeValueCode(String predefinedAllowedAttributeValueCode) {
    this.predefinedAllowedAttributeValueCode = predefinedAllowedAttributeValueCode;
  }

  @Override
  public String toString() {
    return String
        .format(
            "MasterDataProductAttributeValue [descriptiveAttributeValue=%s, descriptiveAttributeValueType=%s, allowedAttributeValueCode=%s, allowedAttributeValue=%s, predefinedAllowedAttributeValueCode=%s, predefinedAllowedAttributeValue=%s, markForDelete=%s, toString()=%s]",
            this.descriptiveAttributeValue, this.descriptiveAttributeValueType,
            this.allowedAttributeValueCode, this.allowedAttributeValue,
            this.predefinedAllowedAttributeValueCode, this.predefinedAllowedAttributeValue,
            this.markForDelete, super.toString());
  }
}
