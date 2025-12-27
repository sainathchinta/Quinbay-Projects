package com.gdn.x.product.domain.event.model;

import java.io.Serializable;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.GdnObjects;

@JsonIgnoreProperties(ignoreUnknown = true)
public class ProductAttribute implements Serializable {

  private static final long serialVersionUID = 8891418032465817909L;

  private String itemSku;

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
