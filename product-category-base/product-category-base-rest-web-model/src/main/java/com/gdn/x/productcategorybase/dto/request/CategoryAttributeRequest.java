package com.gdn.x.productcategorybase.dto.request;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.x.productcategorybase.dto.BaseDTORequest;

@JsonIgnoreProperties(ignoreUnknown = true)
public class CategoryAttributeRequest extends BaseDTORequest {

  private static final long serialVersionUID = -7340331230224833476L;

  private AttributeRequest attribute;

  private Integer sequence;

  private boolean isMainDefiningAttribute = false;

  private boolean isUSP = false;

  public CategoryAttributeRequest() {}

  public CategoryAttributeRequest(AttributeRequest attribute, Integer sequence, boolean isMainDefiningAttribute,
      boolean isUSP, String storeId) {
    this.attribute = attribute;
    this.sequence = sequence;
    this.isMainDefiningAttribute = isMainDefiningAttribute;
    this.isUSP = isUSP;
    this.setStoreId(storeId);
  }

  public AttributeRequest getAttribute() {
    return this.attribute;
  }

  public Integer getSequence() {
    return this.sequence;
  }

  public boolean isMainDefiningAttribute() {
    return this.isMainDefiningAttribute;
  }

  public boolean isUSP() {
    return this.isUSP;
  }

  public void setAttribute(AttributeRequest attribute) {
    this.attribute = attribute;
  }

  public void setMainDefiningAttribute(boolean isMainDefiningAttribute) {
    this.isMainDefiningAttribute = isMainDefiningAttribute;
  }

  public void setSequence(Integer sequence) {
    this.sequence = sequence;
  }

  public void setUSP(boolean isUSP) {
    this.isUSP = isUSP;
  }

  @Override
  public String toString() {
    return String.format(
        "CategoryAttribute [attribute=%s, sequence=%s, isMainDefiningAttribute=%s, isUSP=%s, toString()=%s]",
        this.attribute, this.sequence, this.isMainDefiningAttribute, this.isUSP, super.toString());
  }

}
