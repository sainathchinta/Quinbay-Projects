package com.gdn.x.product.rest.web.model.request;

import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.web.base.BaseRequest;


@JsonIgnoreProperties(ignoreUnknown = true)
public class ProductAttributeRequest extends BaseRequest {
  private static final long serialVersionUID = -2937976123504963277L;
  
  private AttributeRequest attribute;
  private String productAttributeName;
  private boolean isOwnByProductItem;
  private Integer sequence;
  private List<ProductAttributeValueRequest> productAttributeValues;
  
  public AttributeRequest getAttribute() {
    return attribute;
  }
  public void setAttribute(AttributeRequest attribute) {
    this.attribute = attribute;
  }
  public String getProductAttributeName() {
    return productAttributeName;
  }
  public void setProductAttributeName(String productAttributeName) {
    this.productAttributeName = productAttributeName;
  }
  public boolean isOwnByProductItem() {
    return isOwnByProductItem;
  }
  public void setOwnByProductItem(boolean isOwnByProductItem) {
    this.isOwnByProductItem = isOwnByProductItem;
  }
  public Integer getSequence() {
    return sequence;
  }
  public void setSequence(Integer sequence) {
    this.sequence = sequence;
  }
  public List<ProductAttributeValueRequest> getProductAttributeValues() {
    return productAttributeValues;
  }
  public void setProductAttributeValues(List<ProductAttributeValueRequest> productAttributeValues) {
    this.productAttributeValues = productAttributeValues;
  }
}

