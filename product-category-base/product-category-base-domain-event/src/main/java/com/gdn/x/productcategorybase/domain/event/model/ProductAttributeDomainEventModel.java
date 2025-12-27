package com.gdn.x.productcategorybase.domain.event.model;

import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

@JsonIgnoreProperties(ignoreUnknown = true)
public class ProductAttributeDomainEventModel {

  private AttributeDomainEventModel attribute;
  private String productAttributeName;
  private boolean ownByProductItem;
  private Integer sequence;
  private List<ProductAttributeValueDomainEventModel> productAttributeValues;

  public ProductAttributeDomainEventModel() {
    // do nothing
  }

  public ProductAttributeDomainEventModel(AttributeDomainEventModel attribute, String productAttributeName,
      boolean ownByProductItem, Integer sequence, List<ProductAttributeValueDomainEventModel> productAttributeValues) {
    super();
    this.attribute = attribute;
    this.productAttributeName = productAttributeName;
    this.ownByProductItem = ownByProductItem;
    this.sequence = sequence;
    this.productAttributeValues = productAttributeValues;
  }

  public AttributeDomainEventModel getAttribute() {
    return attribute;
  }

  public String getProductAttributeName() {
    return productAttributeName;
  }

  public List<ProductAttributeValueDomainEventModel> getProductAttributeValues() {
    return productAttributeValues;
  }

  public Integer getSequence() {
    return sequence;
  }

  public boolean isOwnByProductItem() {
    return ownByProductItem;
  }

  public void setAttribute(AttributeDomainEventModel attribute) {
    this.attribute = attribute;
  }

  public void setOwnByProductItem(boolean ownByProductItem) {
    this.ownByProductItem = ownByProductItem;
  }

  public void setProductAttributeName(String productAttributeName) {
    this.productAttributeName = productAttributeName;
  }

  public void setProductAttributeValues(List<ProductAttributeValueDomainEventModel> productAttributeValues) {
    this.productAttributeValues = productAttributeValues;
  }

  public void setSequence(Integer sequence) {
    this.sequence = sequence;
  }

  @Override
  public String toString() {
    return String
        .format(
            "ProductAttributeDomainEventModel [attribute=%s, productAttributeName=%s, isOwnByProductItem=%s, sequence=%s, productAttributeValues=%s, getAttribute()=%s, getProductAttributeName()=%s, isOwnByProductItem()=%s, getSequence()=%s, getProductAttributeValues()=%s]",
            attribute, productAttributeName, ownByProductItem, sequence, productAttributeValues, getAttribute(),
            getProductAttributeName(), isOwnByProductItem(), getSequence(), getProductAttributeValues());
  }

}
