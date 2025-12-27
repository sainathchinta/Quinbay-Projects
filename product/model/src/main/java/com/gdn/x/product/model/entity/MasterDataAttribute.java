package com.gdn.x.product.model.entity;


import jakarta.persistence.EnumType;
import jakarta.persistence.Enumerated;
import org.springframework.data.annotation.Transient;
import org.springframework.data.mongodb.core.index.Indexed;
import org.springframework.data.mongodb.core.mapping.Document;
import org.springframework.data.mongodb.core.mapping.Field;

import com.gdn.common.base.GdnObjects;
import com.gdn.x.product.enums.MasterDataAttributeType;
import com.gdn.x.product.enums.ProductFieldNames;


@Document(collection = MasterDataAttribute.DOCUMENT_NAME)
public class MasterDataAttribute extends GdnBaseMongoEntity {

  private static final long serialVersionUID = 1L;

  public static final String DOCUMENT_NAME = "prd_master_data_attribute";

  @Indexed(unique = true)
  @Field(value = ProductFieldNames.ATTRIBUTE_CODE)
  private String attributeCode;

  @Enumerated(EnumType.STRING)
  @Field(value = ProductFieldNames.ATTRIBUTE_TYPE)
  private MasterDataAttributeType attributeType;

  @Field(value = ProductFieldNames.MANDATORY)
  private boolean mandatory;

  @Field(value = ProductFieldNames.ATTRIBUTE_NAME)
  private String attributeName;

  @Field(value = ProductFieldNames.IS_SEARCHABLE)
  private boolean isSearchable;

  @Field(value = ProductFieldNames.DESCRIPTION)
  private String description;

  @Field(value = ProductFieldNames.EXAMPLE)
  private String example;

  @Field(value = ProductFieldNames.IS_SKU_VALUE)
  private boolean isSkuValue;

  @Field(value = ProductFieldNames.VARIANT_CREATION)
  private boolean variantCreation;

  @Field(value = ProductFieldNames.IS_BASIC_VIEW)
  private boolean isBasicView;

  @Transient
  private boolean extractedValue;

  @Transient
  private boolean sizeAttribute;

  @Transient
  private boolean mustShow;

  @Transient
  private boolean hideOnCustomerSide;

  @Transient
  private boolean dsExtraction;

  @Transient
  private boolean hideFromSeller;

  public MasterDataAttribute() {

  }

  public MasterDataAttribute(MasterDataAttributeType attributeType, boolean mandatory,
      String attributeName, boolean isSearchable, String description, String attributeCode,
      String example, boolean isSkuValue, boolean variantCreation) {
    super();
    this.attributeType = attributeType;
    this.mandatory = mandatory;
    this.attributeName = attributeName;
    this.isSearchable = isSearchable;
    this.description = description;
    this.attributeCode = attributeCode;
    this.example = example;
    this.isSkuValue = isSkuValue;
    this.variantCreation = variantCreation;
  }

  @Override
  public boolean equals(Object anObject) {
    return GdnObjects.equals(this, anObject);
  }

  public String getAttributeCode() {
    return this.attributeCode;
  }

  public String getAttributeName() {
    return this.attributeName;
  }

  public MasterDataAttributeType getAttributeType() {
    return this.attributeType;
  }

  public String getDescription() {
    return this.description;
  }

  public String getExample() {
    return this.example;
  }

  @Override
  public int hashCode() {
    return GdnObjects.hashCode(this);
  }

  public boolean isMandatory() {
    return this.mandatory;
  }

  public boolean isSearchable() {
    return this.isSearchable;
  }

  public boolean isSkuValue() {
    return this.isSkuValue;
  }

  public void setAttributeCode(String attributeCode) {
    this.attributeCode = attributeCode;
  }

  public void setAttributeName(String attributeName) {
    this.attributeName = attributeName;
  }

  public void setAttributeType(MasterDataAttributeType attributeType) {
    this.attributeType = attributeType;
  }

  public void setDescription(String description) {
    this.description = description;
  }

  public void setExample(String example) {
    this.example = example;
  }

  public void setMandatory(boolean mandatory) {
    this.mandatory = mandatory;
  }

  public void setSearchable(boolean isSearchable) {
    this.isSearchable = isSearchable;
  }

  public void setSkuValue(boolean isSkuValue) {
    this.isSkuValue = isSkuValue;
  }

  public boolean isVariantCreation() {
    return variantCreation;
  }

  public void setVariantCreation(boolean variantCreation) {
    this.variantCreation = variantCreation;
  }

  public boolean getExtractedValue() {
    return this.extractedValue;
  }

  public void setExtractedValue(boolean extractedValue) {
    this.extractedValue = extractedValue;
  }

  public boolean isBasicView() {
    return isBasicView;
  }

  public void setBasicView(boolean basicView) {
    isBasicView = basicView;
  }

  public boolean isSizeAttribute() {
    return sizeAttribute;
  }

  public void setSizeAttribute(boolean sizeAttribute) {
    this.sizeAttribute = sizeAttribute;
  }

  public boolean isMustShow() {
    return mustShow;
  }

  public void setMustShow(boolean mustShow) {
    this.mustShow = mustShow;
  }

  public boolean isHideOnCustomerSide() {
    return hideOnCustomerSide;
  }

  public void setHideOnCustomerSide(boolean hideOnCustomerSide) {
    this.hideOnCustomerSide = hideOnCustomerSide;
  }

  public boolean isDsExtraction() {
    return dsExtraction;
  }

  public void setDsExtraction(boolean dsExtraction) {
    this.dsExtraction = dsExtraction;
  }

  public boolean isHideFromSeller() {
    return hideFromSeller;
  }

  public void setHideFromSeller(boolean hideFromSeller) {
    this.hideFromSeller = hideFromSeller;
  }

  @Override
  public String toString() {
    return String.format("MasterDataAttribute [attributeType=%s, mandatory=%s, attributeName=%s, "
            + "isSearchable=%s, description=%s, attributeCode=%s, example=%s, isSkuValue=%s, "
            + "isVariantCreation=%s, isBasicView=%s,extractedValue=%s, sizeAttribute=%s, hideForCustomer=%s, dsExtraction=%s, hideFromSeller=%s, toString()=%s]",
        this.attributeType, this.mandatory, this.attributeName, this.isSearchable, this.description, this.attributeCode,
        this.example, this.isSkuValue, this.variantCreation, this.isBasicView, this.extractedValue, this.sizeAttribute,
        this.hideOnCustomerSide, this.dsExtraction, this.hideFromSeller, super.toString());
  }
}
