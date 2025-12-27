package com.gdn.x.product.domain.event.model;

import java.io.Serializable;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.GdnObjects;

@JsonIgnoreProperties(ignoreUnknown = true)
public class MasterDataItemAttributeValue implements Serializable {

  private static final long serialVersionUID = 3622239550029969451L;

  private String attributeValue;

  private MasterDataAttribute masterDataAttribute;

  public MasterDataItemAttributeValue() {

  }

  public MasterDataItemAttributeValue(String attributeValue, MasterDataAttribute masterDataAttribute) {
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

  public MasterDataAttribute getMasterDataAttribute() {
    return this.masterDataAttribute;
  }

  @Override
  public int hashCode() {
    return GdnObjects.hashCode(this);
  }

  public void setAttributeValue(String attributeValue) {
    this.attributeValue = attributeValue;
  }

  public void setMasterDataAttribute(MasterDataAttribute masterDataAttribute) {
    this.masterDataAttribute = masterDataAttribute;
  }

  @Override
  public String toString() {
    return String.format(
        "MasterDataItemAttributeValue [attributeValue=%s, masterDataAttribute=%s, toString()=%s]",
        this.attributeValue, this.masterDataAttribute, super.toString());
  }
}
