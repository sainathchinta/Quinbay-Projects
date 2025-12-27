package com.gdn.x.productcategorybase.dto.request;

import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.web.base.BaseRequest;

@JsonIgnoreProperties(ignoreUnknown = true)
public class AddProductAttributesRequest extends BaseRequest {
  private static final long serialVersionUID = -605190898814771295L;
  private String productCode;
  private boolean isOwnByProductItem;
  private Integer sequence;
  private List<NewAttributeRequest> newAttributes;
  
  public AddProductAttributesRequest() {
    super();
  }
  public String getProductCode() {
    return productCode;
  }
  public void setProductCode(String productCode) {
    this.productCode = productCode;
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
  public List<NewAttributeRequest> getNewAttributes() {
    return newAttributes;
  }
  public void setNewAttributes(List<NewAttributeRequest> newAttributes) {
    this.newAttributes = newAttributes;
  }
  
  
}
