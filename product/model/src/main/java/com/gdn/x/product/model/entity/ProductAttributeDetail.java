package com.gdn.x.product.model.entity;

import org.apache.commons.lang3.StringUtils;
import org.springframework.data.annotation.Transient;
import org.springframework.data.mongodb.core.mapping.Field;

import com.gdn.common.base.GdnObjects;
import com.gdn.x.product.enums.ProductFieldNames;

import lombok.Builder;

@Builder
public class ProductAttributeDetail implements GdnBaseEmbedded {

  private static final long serialVersionUID = 1L;

  @Field(value = ProductFieldNames.ATTRIBUTE_CODE)
  private String attributeCode;

  @Field(value = ProductFieldNames.ATTRIBUTE_NAME)
  private String attributeName;

  @Field(value = ProductFieldNames.ATTRIBUTE_VALUE)
  private String attributeValue;

  @Transient
  private boolean mustShow;

  @Transient
  private String attributeValueEnglish;

  @Transient
  private String attributeType = StringUtils.EMPTY;

  public ProductAttributeDetail() {

  }

  public ProductAttributeDetail(String attributeCode, String attributeName, String attributeValue, boolean mustShow) {
    this.attributeCode = attributeCode;
    this.attributeName = attributeName;
    this.attributeValue = attributeValue;
    this.mustShow = mustShow;
  }

  public ProductAttributeDetail(String attributeCode, String attributeName, String attributeValue, boolean mustShow, String attributeValueEnglish) {
    this.attributeCode = attributeCode;
    this.attributeName = attributeName;
    this.attributeValue = attributeValue;
    this.mustShow = mustShow;
    this.attributeValueEnglish = attributeValueEnglish;
  }

  public ProductAttributeDetail(String attributeCode, String attributeName, String attributeValue, boolean mustShow,
      String attributeValueEnglish, String attributeType) {
    this.attributeCode = attributeCode;
    this.attributeName = attributeName;
    this.attributeValue = attributeValue;
    this.mustShow = mustShow;
    this.attributeValueEnglish = attributeValueEnglish;
    this.attributeType = attributeType;
  }

  public ProductAttributeDetail(String attributeCode, String attributeName, String attributeValue) {
    super();
    this.attributeCode = attributeCode;
    this.attributeName = attributeName;
    this.attributeValue = attributeValue;
  }

  public ProductAttributeDetail(String attributeCode, String attributeName, String attributeValue,
      String attributeValueEnglish) {
    super();
    this.attributeCode = attributeCode;
    this.attributeName = attributeName;
    this.attributeValue = attributeValue;
    this.attributeValueEnglish = attributeValueEnglish;
  }

  @Override
  public boolean equals(Object obj) {
    return GdnObjects.equals(this, obj);
  }

  public String getAttributeCode() {
    return this.attributeCode;
  }

  public String getAttributeName() {
    return this.attributeName;
  }

  public String getAttributeValue() {
    return this.attributeValue;
  }

  @Override
  public int hashCode() {
    return GdnObjects.hashCode(this);
  }

  public void setAttributeCode(String attributeCode) {
    this.attributeCode = attributeCode;
  }

  public void setAttributeName(String attributeName) {
    this.attributeName = attributeName;
  }

  public void setAttributeValue(String attributeValue) {
    this.attributeValue = attributeValue;
  }

  public boolean isMustShow() {
    return mustShow;
  }

  public void setMustShow(boolean mustShow) {
    this.mustShow = mustShow;
  }

  public String getattributeValueEnglish() {
    return attributeValueEnglish;
  }

  public void setattributeValueEnglish(String attributeValueEnglish) {
    this.attributeValueEnglish = attributeValueEnglish;
  }

  public String getAttributeType() {
    return attributeType;
  }

  public void setAttributeType(String attributeType) {
    this.attributeType = attributeType;
  }

  @Override
  public String toString() {
    return String
        .format(
            "ProductAttributeDetail [attributeCode=%s, attributeName=%s, attributeValue=%s, mustShow=%s toString()=%s]",
            this.attributeCode, this.attributeName, this.attributeValue, this.mustShow, super.toString());
  }
}
