package com.gdn.x.mta.distributiontask.model;

import com.fasterxml.jackson.annotation.JsonIgnore;
import lombok.ToString;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.JoinColumn;
import jakarta.persistence.ManyToOne;
import jakarta.persistence.Table;

@Table(name = "PDT_PRODUCT_ITEM_ATTRIBUTE")
@Entity
public class ProductItemAttribute extends GdnBaseEntity {
  private static final long serialVersionUID = 1L;

  @Column(name = "ATTRIBUTE_CODE")
  private String attributeCode;

  @Column(name = "NAME")
  private String name;

  @Column(name = "VALUE")
  private String value;

  @ToString.Exclude
  @JoinColumn(name = "PRODUCT_ITEM")
  @ManyToOne
  @JsonIgnore
  private ProductItem productItem;

  @Column(name = "ATTRIBUTE_TYPE")
  private String attributeType;

  @Column(name = "VARIANT_CREATION")
  private boolean variantCreation;

  public ProductItemAttribute() {}

  public ProductItemAttribute(String storeId, String attributeCode, String name, String value,
      String attributeType) {
    this.setStoreId(storeId);
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

  public ProductItem getProductItem() {
    return productItem;
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

  public void setProduct(ProductItem productItem) {
    this.productItem = productItem;
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
    builder.append("]");
    return builder.toString();
  }


}
