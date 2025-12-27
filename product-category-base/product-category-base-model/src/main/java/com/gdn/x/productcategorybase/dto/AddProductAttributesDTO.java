package com.gdn.x.productcategorybase.dto;

import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

@JsonIgnoreProperties(ignoreUnknown = true)
public class AddProductAttributesDTO {
  private String productCode;
  private boolean isOwnByProductItem;
  private Integer sequence;
  private List<NewAttributeRequestDTO> newAttributes;
  
  public AddProductAttributesDTO() {
    super();
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
  public String getProductCode() {
    return productCode;
  }
  public void setProductCode(String productCode) {
    this.productCode = productCode;
  }
  public List<NewAttributeRequestDTO> getNewAttributes() {
    return newAttributes;
  }
  public void setNewAttributes(List<NewAttributeRequestDTO> newAttributes) {
    this.newAttributes = newAttributes;
  }
}
