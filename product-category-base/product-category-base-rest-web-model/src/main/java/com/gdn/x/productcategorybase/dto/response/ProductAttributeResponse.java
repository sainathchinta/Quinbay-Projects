package com.gdn.x.productcategorybase.dto.response;

import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.x.productcategorybase.dto.BaseDTOResponse;

@JsonIgnoreProperties(ignoreUnknown = true)
public class ProductAttributeResponse extends BaseDTOResponse {

  private static final long serialVersionUID = 8035954552229409923L;
  private AttributeResponse attribute;
  private String productAttributeName;
  private boolean isOwnByProductItem;
  private Integer sequence;
  private List<ProductAttributeValueResponse> productAttributeValues;

  public ProductAttributeResponse() {
    // TODO nothing to do here
  }

  public AttributeResponse getAttribute() {
    return this.attribute;
  }

  public String getProductAttributeName() {
    return this.productAttributeName;
  }

  public List<ProductAttributeValueResponse> getProductAttributeValues() {
    return this.productAttributeValues;
  }

  public Integer getSequence() {
    return this.sequence;
  }

  public boolean isOwnByProductItem() {
    return this.isOwnByProductItem;
  }

  public void setAttribute(AttributeResponse attribute) {
    this.attribute = attribute;
  }

  public void setOwnByProductItem(boolean isOwnByProductItem) {
    this.isOwnByProductItem = isOwnByProductItem;
  }

  public void setProductAttributeName(String productAttributeName) {
    this.productAttributeName = productAttributeName;
  }

  public void setProductAttributeValues(List<ProductAttributeValueResponse> productAttributeValues) {
    this.productAttributeValues = productAttributeValues;
  }

  public void setSequence(Integer sequence) {
    this.sequence = sequence;
  }

}
