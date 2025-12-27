package com.gdn.x.product.model.entity;

import org.springframework.data.annotation.Transient;
import org.springframework.data.mongodb.core.mapping.DBRef;
import org.springframework.data.mongodb.core.mapping.Field;

import com.gdn.common.base.GdnObjects;
import com.gdn.x.product.enums.ProductFieldNames;

public class MasterDataItemAttributeValue implements GdnBaseEmbedded {

  private static final long serialVersionUID = 1L;

  @Field(value = ProductFieldNames.ATTRIBUTE_VALUE)
  private String attributeValue;

  @Field(value = ProductFieldNames.MARK_FOR_DELETE)
  private boolean markForDelete;

  @DBRef
  private MasterDataAttribute masterDataAttribute;

  @Transient
  private String valueType;

  public String getValueType() {
    return valueType;
  }

  public void setValueType(String valueType) {
    this.valueType = valueType;
  }

  public MasterDataItemAttributeValue() {

  }

  public MasterDataItemAttributeValue(String attributeValue, boolean markForDelete,
      MasterDataAttribute masterDataAttribute) {
    this.attributeValue = attributeValue;
    this.markForDelete = markForDelete;
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

  public boolean isMarkForDelete() {
    return markForDelete;
  }

  public void setAttributeValue(String attributeValue) {
    this.attributeValue = attributeValue;
  }

  public void setMarkForDelete(boolean markForDelete) {
    this.markForDelete = markForDelete;
  }

  public void setMasterDataAttribute(MasterDataAttribute masterDataAttribute) {
    this.masterDataAttribute = masterDataAttribute;
  }

  @Override
  public String toString() {
    return String
        .format(
            "MasterDataItemAttributeValue [attributeValue=%s, markForDelete=%s, masterDataAttribute=%s, toString()=%s]",
            attributeValue, markForDelete, masterDataAttribute, super.toString());
  }
}
