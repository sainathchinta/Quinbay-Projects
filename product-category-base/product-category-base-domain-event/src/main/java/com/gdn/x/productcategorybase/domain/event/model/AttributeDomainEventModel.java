package com.gdn.x.productcategorybase.domain.event.model;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.entity.GdnBaseDomainEventModel;

@JsonIgnoreProperties(ignoreUnknown = true)
public class AttributeDomainEventModel extends GdnBaseDomainEventModel {

  private String id;
  private String name;
  private String attributeCode;
  private String attributeType;
  private byte[] description;
  private boolean skuValue;
  private boolean newAttribute;
  private boolean valueUpdate;
  private boolean variantCreation;
  private boolean mustShowOnCustomerSide;
  private String dsAttributeName;
  private List<String> updatedFields = new ArrayList<>();
  private boolean dsExtraction;

  public void setDsAttributeName(String dsAttributeName) {
    this.dsAttributeName = dsAttributeName;
  }

  public void setUpdatedFields(List<String> updatedFields) {
    this.updatedFields = updatedFields;
  }

  public void setDsExtraction(boolean dsExtraction) {
    this.dsExtraction = dsExtraction;
  }

  public AttributeDomainEventModel() {
    // do nothing
  }

  public AttributeDomainEventModel(String name, String attributeCode, String attributeType, byte[] description,
      boolean skuValue) {
    super();
    this.name = name;
    this.attributeCode = attributeCode;
    this.attributeType = attributeType;
    if (description != null) {
      this.description = description.clone();
    }
    this.skuValue = skuValue;
  }

  public String getAttributeCode() {
    return attributeCode;
  }

  public String getAttributeType() {
    return attributeType;
  }

  public byte[] getDescription() {
    return description;
  }

  public String getName() {
    return name;
  }

  public boolean isSkuValue() {
    return skuValue;
  }

  public void setAttributeCode(String attributeCode) {
    this.attributeCode = attributeCode;
  }

  public void setAttributeType(String attributeType) {
    this.attributeType = attributeType;
  }

  public void setDescription(byte[] description) {
    this.description = description;
  }

  public void setName(String name) {
    this.name = name;
  }

  public void setSkuValue(boolean skuValue) {
    this.skuValue = skuValue;
  }


  public String getId() {
    return id;
  }

  public void setId(String id) {
    this.id = id;
  }

  public boolean isNewAttribute() {
    return newAttribute;
  }

  public void setNewAttribute(boolean newAttribute) {
    this.newAttribute = newAttribute;
  }

  public boolean isValueUpdate() {
    return valueUpdate;
  }

  public void setValueUpdate(boolean valueUpdate) {
    this.valueUpdate = valueUpdate;
  }

  public boolean isVariantCreation() {
    return variantCreation;
  }

  public void setVariantCreation(boolean variantCreation) {
    this.variantCreation = variantCreation;
  }

  public boolean isMustShowOnCustomerSide() {
    return mustShowOnCustomerSide;
  }

  public void setMustShowOnCustomerSide(boolean mustShowOnCustomerSide) {
    this.mustShowOnCustomerSide = mustShowOnCustomerSide;
  }

  public String getDsAttributeName() {
    return dsAttributeName;
  }

  public List<String> getUpdatedFields() {
    return updatedFields;
  }

  public boolean isDsExtraction() {
    return dsExtraction;
  }

  @Override
  public String toString() {
    return String.format(
        "AttributeDomainEventModel [id=%s, name=%s, attributeCode=%s, attributeType=%s, description=%s, skuValue=%s, variantCreation=%s, isNewAttribute=%s, isValueUpdate=%s, getId()=%s, getName()=%s, getAttributeCode()=%s, getAttributeType()=%s, getDescription()=%s, isSkuValue()=%s, isNewAttribute()=%s, isValueUpdate()=%s, isVariantCreation=%s, getDsAttributeName()=%s, isDsExtraction()=%s, getUpdatedFields()=%s]",
        id, name, attributeCode, attributeType, Arrays.toString(description), skuValue, newAttribute, valueUpdate, variantCreation, dsExtraction, updatedFields, dsAttributeName,
        getId(), getName(), getAttributeCode(), getAttributeType(), Arrays.toString(getDescription()), isSkuValue(),
        isNewAttribute(), isValueUpdate(), isVariantCreation(), getDsAttributeName(), isDsExtraction(), getUpdatedFields());
  }
}
