package com.gdn.x.productcategorybase.dto.response;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.x.productcategorybase.dto.BaseDTOResponse;

@JsonIgnoreProperties(ignoreUnknown = true)
public class CategoryAttributeResponse extends BaseDTOResponse {

  private static final long serialVersionUID = -7340331230224833476L;

  private AttributeResponse attribute;

  private Integer sequence;

  private boolean isMainDefiningAttribute = false;

  private boolean isUSP = false;

  private boolean mainAttributeFlag;

  public CategoryAttributeResponse() {}

  public CategoryAttributeResponse(AttributeResponse attribute, Integer sequence, boolean isMainDefiningAttribute,
      boolean isUSP, String storeId) {
    this.attribute = attribute;
    this.sequence = sequence;
    this.isMainDefiningAttribute = isMainDefiningAttribute;
    this.isUSP = isUSP;
    this.setStoreId(storeId);
  }

  public AttributeResponse getAttribute() {
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

  public void setAttribute(AttributeResponse attribute) {
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

  public boolean isMainAttributeFlag() {
    return mainAttributeFlag;
  }

  public void setMainAttributeFlag(boolean mainAttributeFlag) {
    this.mainAttributeFlag = mainAttributeFlag;
  }

  @Override
  public String toString() {
    return String.format(
        "CategoryAttribute [attribute=%s, sequence=%s, isMainDefiningAttribute=%s, isUSP=%s, mainAttributeFlag=%s, toString()=%s]",
        this.attribute, this.sequence, this.isMainDefiningAttribute, this.isUSP,this.mainAttributeFlag,  super.toString());
  }

}
