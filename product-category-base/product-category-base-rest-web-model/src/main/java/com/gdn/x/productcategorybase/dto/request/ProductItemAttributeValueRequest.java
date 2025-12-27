package com.gdn.x.productcategorybase.dto.request;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.x.productcategorybase.dto.BaseDTORequest;

@JsonIgnoreProperties(ignoreUnknown = true)
public class ProductItemAttributeValueRequest extends BaseDTORequest {

  private static final long serialVersionUID = 7129832002021805773L;

  private AttributeRequest attribute;
  private String value;

  public ProductItemAttributeValueRequest() {}

  public ProductItemAttributeValueRequest(AttributeRequest attribute, String value, String storeId) {
    this.attribute = attribute;
    this.value = value;
    this.setStoreId(storeId);
  }

  public AttributeRequest getAttribute() {
    return this.attribute;
  }

  public String getValue() {
    return this.value;
  }

  public void setAttribute(AttributeRequest attribute) {
    this.attribute = attribute;
  }

  public void setValue(String value) {
    this.value = value;
  }

  @Override
  public String toString() {
    return String.format("ProductItemAttributeValue [attribute=%s, value=%s, toString()=%s]", this.attribute,
        this.value, super.toString());
  }

}
