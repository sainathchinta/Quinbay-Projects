package com.gdn.x.product.model.entity;

import java.util.List;

import org.springframework.data.mongodb.core.mapping.Field;

import com.gdn.common.base.GdnObjects;
import com.gdn.x.product.enums.ProductFieldNames;

public class ProductAttribute implements GdnBaseEmbedded {

  private static final long serialVersionUID = 1L;

  @Field(value = ProductFieldNames.ITEM_SKU)
  private String itemSku;

  @Field(value = ProductFieldNames.PRODUCT_ATTRIBUTE_DETAILS)
  private List<ProductAttributeDetail> productAttributeDetails;

  public ProductAttribute() {

  }

  public ProductAttribute(String itemSku, List<ProductAttributeDetail> productAttributeDetails) {
    super();
    this.itemSku = itemSku;
    this.productAttributeDetails = productAttributeDetails;
  }

  @Override
  public boolean equals(Object obj) {
    return GdnObjects.equals(this, obj);
  }

  public String getItemSku() {
    return this.itemSku;
  }

  public List<ProductAttributeDetail> getProductAttributeDetails() {
    return this.productAttributeDetails;
  }

  @Override
  public int hashCode() {
    return GdnObjects.hashCode(this);
  }

  public void setItemSku(String itemSku) {
    this.itemSku = itemSku;
  }

  public void setProductAttributeDetails(List<ProductAttributeDetail> productAttributeDetails) {
    this.productAttributeDetails = productAttributeDetails;
  }

  @Override
  public String toString() {
    return String.format("Attribute [itemSku=%s, productAttributeDetails=%s, toString()=%s]",
        this.itemSku, this.productAttributeDetails, super.toString());
  }
}
