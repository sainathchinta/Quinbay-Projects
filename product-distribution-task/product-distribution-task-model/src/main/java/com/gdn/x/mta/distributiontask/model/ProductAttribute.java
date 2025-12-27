package com.gdn.x.mta.distributiontask.model;

import com.fasterxml.jackson.annotation.JsonIgnore;
import lombok.ToString;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.JoinColumn;
import jakarta.persistence.ManyToOne;
import jakarta.persistence.Table;

@Table(name = "PDT_PRODUCT_ATTRIBUTE")
@Entity
public class ProductAttribute extends GdnBaseEntity {
  private static final long serialVersionUID = 1L;

  @Column(name = "ATTRIBUTE_CODE")
  private String attributeCode;

  @Column(name = "NAME")
  private String name;

  @Column(name = "VALUE")
  private String value;

  @ToString.Exclude
  @JoinColumn(name = "PRODUCT")
  @ManyToOne
  @JsonIgnore
  private Product product;

  @Column(name = "ATTRIBUTE_TYPE")
  private String attributeType;

  @Column(name = "VARIANT_CREATION")
  private boolean variantCreation;

  @Column(name = "EXTRACTED_VALUE")
  private boolean extractedValue;

  @Column(name = "DS_EXTRACTION")
  private boolean dsExtraction;

  public ProductAttribute() {}

  public ProductAttribute(Product product, String attributeCode, String name, String value,
      String attributeType) {
    this.product = product;
    this.attributeCode = attributeCode;
    this.name = name;
    this.value = value;
    this.attributeType = attributeType;
  }

  public String getAttributeCode() {
    return attributeCode;
  }

  public String getAttributeType() {
    return attributeType;
  }

  public String getName() {
    return name;
  }

  public Product getProduct() {
    return product;
  }

  public String getValue() {
    return value;
  }

  public void setAttributeCode(String attributeCode) {
    this.attributeCode = attributeCode;
  }

  public void setAttributeType(String attributeType) {
    this.attributeType = attributeType;
  }

  public void setName(String name) {
    this.name = name;
  }

  public void setProduct(Product product) {
    this.product = product;
  }

  public void setValue(String value) {
    this.value = value;
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

  public boolean isDsExtraction() {
    return dsExtraction;
  }

  public void setDsExtraction(boolean dsExtraction) {
    this.dsExtraction = dsExtraction;
  }


  @Override
  public String toString() {
    StringBuilder builder = new StringBuilder();
    builder.append("ProductAttribute [attributeCode=");
    builder.append(attributeCode);
    builder.append(", name=");
    builder.append(name);
    builder.append(", value=");
    builder.append(value);
    builder.append(", attributeType=");
    builder.append(attributeType);
    builder.append(", variantCreation=");
    builder.append(variantCreation);
    builder.append(", extractedValue=");
    builder.append(extractedValue);
    builder.append(", dsExtraction=");
    builder.append(dsExtraction);
    builder.append("]");
    return builder.toString();
  }


}
