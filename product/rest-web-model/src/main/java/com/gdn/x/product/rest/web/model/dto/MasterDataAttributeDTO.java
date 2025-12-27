package com.gdn.x.product.rest.web.model.dto;

import java.io.Serializable;


import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.GdnObjects;
import com.gdn.x.product.enums.MasterDataAttributeType;

@JsonIgnoreProperties(ignoreUnknown = true)
public class MasterDataAttributeDTO implements Serializable {

  private static final long serialVersionUID = 1L;

  private String id;
  private MasterDataAttributeType attributeType;
  private boolean mandatory;
  private String attributeName;
  private boolean isSearchable;
  private String description;
  private String attributeCode;
  private String example;
  private boolean isSkuValue;
  private boolean isBasicView;
  private boolean variantCreation;
  private boolean extractedValue;
  private boolean sizeAttribute;
  private boolean mustShow;
  private boolean dsExtraction;
  private boolean hideFromSeller;

  public MasterDataAttributeDTO() {

  }


  public MasterDataAttributeDTO(MasterDataAttributeType attributeType, boolean mandatory,
      String attributeName, boolean isSearchable, String description, String attributeCode,
      String example, boolean isSkuValue) {
    super();
    this.attributeType = attributeType;
    this.mandatory = mandatory;
    this.attributeName = attributeName;
    this.isSearchable = isSearchable;
    this.description = description;
    this.attributeCode = attributeCode;
    this.example = example;
    this.isSkuValue = isSkuValue;
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

  public String getId() {
    return this.id;
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

  public void setId(String id) {
    this.id = id;
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

  public boolean isBasicView() {
    return isBasicView;
  }

  public void setBasicView(boolean basicView) {
    isBasicView = basicView;
  }

  public boolean isVariantCreation() {
    return variantCreation;
  }

  public void setVariantCreation(boolean variantCreation) {
    this.variantCreation = variantCreation;
  }

  public boolean isExtractedValue() {
    return extractedValue;
  }

  public void setExtractedValue(boolean extractedValue) {
    this.extractedValue = extractedValue;
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
    return String.format(
        "MasterDataAttributeDTO [id=%s, attributeType=%s, mandatory=%s, attributeName=%s, isSearchable=%s, description=%s, attributeCode=%s, example=%s, isSkuValue=%s, isBasicView=%s, variantCreation=%s, extractedValue=%s, sizeAttribute=%s, dsExtraction=%s,hideFromSeller=%s,toString()=%s]",
        this.id, this.attributeType, this.mandatory, this.attributeName, this.isSearchable, this.description,
        this.attributeCode, this.example, this.isSkuValue, this.isBasicView, this.variantCreation, this.extractedValue,
        this.sizeAttribute, this.dsExtraction, this.hideFromSeller, super.toString());
  }
}
