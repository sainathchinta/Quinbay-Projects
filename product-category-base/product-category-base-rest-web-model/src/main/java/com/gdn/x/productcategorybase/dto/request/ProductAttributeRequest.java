package com.gdn.x.productcategorybase.dto.request;

import java.util.ArrayList;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.x.productcategorybase.dto.BaseDTORequest;

@JsonIgnoreProperties(ignoreUnknown = true)
public class ProductAttributeRequest extends BaseDTORequest {

  private static final long serialVersionUID = 7836061834774566904L;

  private AttributeRequest attribute;
  private String productAttributeName;
  private boolean isOwnByProductItem;
  private Integer sequence;
  private List<ProductAttributeValueRequest> productAttributeValues = new ArrayList<ProductAttributeValueRequest>();

  public ProductAttributeRequest() {}

  public ProductAttributeRequest(AttributeRequest attribute, String productAttributeName, boolean isOwnByProductItem,
      Integer sequence, String storeId) {
    this.attribute = attribute;
    this.productAttributeName = productAttributeName;
    this.isOwnByProductItem = isOwnByProductItem;
    this.sequence = sequence;
    this.setStoreId(storeId);
  }

  public AttributeRequest getAttribute() {
    return this.attribute;
  }

  public String getProductAttributeName() {
    return this.productAttributeName;
  }

  public List<ProductAttributeValueRequest> getProductAttributeValues() {
    return this.productAttributeValues;
  }

  public Integer getSequence() {
    return this.sequence;
  }

  public boolean isOwnByProductItem() {
    return this.isOwnByProductItem;
  }

  public void setAttribute(AttributeRequest attribute) {
    this.attribute = attribute;
  }

  public void setOwnByProductItem(boolean isOwnByProductItem) {
    this.isOwnByProductItem = isOwnByProductItem;
  }

  public void setProductAttributeName(String productAttributeName) {
    this.productAttributeName = productAttributeName;
  }

  public void setProductAttributeValues(List<ProductAttributeValueRequest> productAttributeValues) {
    this.productAttributeValues = productAttributeValues;
  }

  public void setSequence(Integer sequence) {
    this.sequence = sequence;
  }

  @Override
  public String toString() {
    return String.format(
        "ProductAttributeRequest [attribute=%s, productAttributeName=%s, isOwnByProductItem=%s, sequence=%s, productAttributeValues=%s, toString()=%s]",
        this.attribute, this.productAttributeName, this.isOwnByProductItem, this.sequence, this.productAttributeValues,
        super.toString());
  }
}
