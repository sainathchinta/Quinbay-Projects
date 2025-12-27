package com.gdn.x.product.domain.event.model;

import java.io.Serializable;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.GdnObjects;

@JsonIgnoreProperties(ignoreUnknown = true)
public class ProductAttributeDetail implements Serializable {

  private static final long serialVersionUID = 7436151332802923470L;

  private String attributeCode;

  private String attributeName;

  private String attributeValue;

  private String sequence;

  public ProductAttributeDetail() {

  }

  public ProductAttributeDetail(String attributeCode, String attributeName, String attributeValue,
      String sequence) {
    super();
    this.attributeCode = attributeCode;
    this.attributeName = attributeName;
    this.attributeValue = attributeValue;
    this.sequence = sequence;
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

  @Deprecated
  /**
   * @deprecated field sequence is unused
   */
  public String getSequence() {
    return this.sequence;
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

  @Deprecated
  /**
   * @deprecated field sequence is unused
   */
  public void setSequence(String sequence) {
    this.sequence = sequence;
  }

  @Override
  public String toString() {
    return String.format(
        "ProductAttributeDetail [attributeCode=%s, attributeName=%s, attributeValue=%s, sequence=%s, toString()=%s]",
        this.attributeCode, this.attributeName, this.attributeValue, this.sequence,
        super.toString());
  }
}
