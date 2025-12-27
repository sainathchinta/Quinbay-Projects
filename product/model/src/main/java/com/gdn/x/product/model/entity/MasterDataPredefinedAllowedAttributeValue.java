package com.gdn.x.product.model.entity;

import com.gdn.common.base.GdnObjects;

public class MasterDataPredefinedAllowedAttributeValue implements GdnBaseEmbedded {

  private static final long serialVersionUID = 1L;

  private String predefinedAllowedAttributeValueCode;

  private String attributeValue;

  private Integer sequence;

  public MasterDataPredefinedAllowedAttributeValue() {

  }

  public MasterDataPredefinedAllowedAttributeValue(String predefinedAllowedAttributeValueCode,
      String attributeValue, Integer sequence) {
    super();
    this.predefinedAllowedAttributeValueCode = predefinedAllowedAttributeValueCode;
    this.attributeValue = attributeValue;
    this.sequence = sequence;
  }

  @Override
  public boolean equals(Object obj) {
    return GdnObjects.equals(this, obj);
  }

  public String getAttributeValue() {
    return this.attributeValue;
  }

  public String getPredefinedAllowedAttributeValueCode() {
    return this.predefinedAllowedAttributeValueCode;
  }

  public Integer getSequence() {
    return this.sequence;
  }

  @Override
  public int hashCode() {
    return GdnObjects.hashCode(this);
  }

  public void setAttributeValue(String attributeValue) {
    this.attributeValue = attributeValue;
  }

  public void setPredefinedAllowedAttributeValueCode(String predefinedAllowedAttributeValueCode) {
    this.predefinedAllowedAttributeValueCode = predefinedAllowedAttributeValueCode;
  }

  public void setSequence(Integer sequence) {
    this.sequence = sequence;
  }

  @Override
  public String toString() {
    return String.format(
        "PredefinedAllowedAttributeValue [predefinedAllowedAttributeValueCode=%s, attributeValue=%s, sequence=%s, toString()=%s]",
        this.predefinedAllowedAttributeValueCode, this.attributeValue, this.sequence,
        super.toString());
  }
}
