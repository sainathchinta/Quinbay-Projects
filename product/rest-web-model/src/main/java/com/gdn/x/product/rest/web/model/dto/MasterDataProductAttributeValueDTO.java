package com.gdn.x.product.rest.web.model.dto;

import java.io.Serializable;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.GdnObjects;
import com.gdn.x.product.enums.DescriptiveAttributeValueType;

@JsonIgnoreProperties(ignoreUnknown = true)
public class MasterDataProductAttributeValueDTO implements Serializable {
  private static final long serialVersionUID = 1L;

  private String descriptiveAttributeValue;
  private DescriptiveAttributeValueType descriptiveAttributeValueType;
  private PredefinedAllowedAttributeValueDTO predefinedAllowedAttributeValue;
  private MasterDataAllowedAttributeValueDTO allowedAttributeValue;
  private int sequence;
  private boolean markForDelete;

  public MasterDataProductAttributeValueDTO() {

  }

  public MasterDataProductAttributeValueDTO(String descriptiveAttributeValue,
      DescriptiveAttributeValueType descriptiveAttributeValueType,
      PredefinedAllowedAttributeValueDTO predefinedAllowedAttributeValue, int sequence,
      boolean markForDelete) {
    this.descriptiveAttributeValue = descriptiveAttributeValue;
    this.descriptiveAttributeValueType = descriptiveAttributeValueType;
    this.predefinedAllowedAttributeValue = predefinedAllowedAttributeValue;
    this.sequence = sequence;
    this.markForDelete = markForDelete;
  }

  @Override
  public boolean equals(Object anObject) {
    return GdnObjects.equals(this, anObject);
  }

  public MasterDataAllowedAttributeValueDTO getAllowedAttributeValue() {
    return allowedAttributeValue;
  }

  public String getDescriptiveAttributeValue() {
    return this.descriptiveAttributeValue;
  }

  public DescriptiveAttributeValueType getDescriptiveAttributeValueType() {
    return this.descriptiveAttributeValueType;
  }

  public PredefinedAllowedAttributeValueDTO getPredefinedAllowedAttributeValue() {
    return predefinedAllowedAttributeValue;
  }

  public int getSequence() {
    return this.sequence;
  }

  @Override
  public int hashCode() {
    return GdnObjects.hashCode(this);
  }

  public boolean isMarkForDelete() {
    return markForDelete;
  }

  public void setAllowedAttributeValue(MasterDataAllowedAttributeValueDTO allowedAttributeValue) {
    this.allowedAttributeValue = allowedAttributeValue;
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
      PredefinedAllowedAttributeValueDTO predefinedAllowedAttributeValue) {
    this.predefinedAllowedAttributeValue = predefinedAllowedAttributeValue;
  }

  public void setSequence(int sequence) {
    this.sequence = sequence;
  }

  @Override
  public String toString() {
    return String
        .format(
            "MasterDataProductAttributeValueDTO [descriptiveAttributeValue=%s, descriptiveAttributeValueType=%s, predefinedAllowedAttributeValue=%s, allowedAttributeValue=%s, sequence=%s, markForDelete=%s, toString()=%s]",
            descriptiveAttributeValue, descriptiveAttributeValueType,
            predefinedAllowedAttributeValue, allowedAttributeValue, sequence, markForDelete,
            super.toString());
  }

}
