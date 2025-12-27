package com.gdn.x.product.model.vo;

import com.gdn.common.base.GdnObjects;
import org.apache.commons.lang3.builder.ToStringBuilder;

import java.io.Serializable;

public class MasterDataItemAttributeVO implements Serializable {

  private static final long serialVersionUID = 1L;

  private String attributeValue;
  private String attributeCode;
  private String attributeName;

  public MasterDataItemAttributeVO() {
  }

  public MasterDataItemAttributeVO(String attributeValue, String attributeCode,
      String attributeName) {
    this.attributeValue = attributeValue;
    this.attributeCode = attributeCode;
    this.attributeName = attributeName;
  }

  @Override
  public boolean equals(Object obj) {
    return GdnObjects.equals(this, obj);
  }

  public String getAttributeCode() {
    return attributeCode;
  }

  public String getAttributeName() {
    return attributeName;
  }

  public String getAttributeValue() {
    return attributeValue;
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

  @Override
  public String toString() {
    return new ToStringBuilder(this).append("attributeValue", attributeValue)
        .append("attributeCode", attributeCode).append("attributeName", attributeName).toString();
  }
}
