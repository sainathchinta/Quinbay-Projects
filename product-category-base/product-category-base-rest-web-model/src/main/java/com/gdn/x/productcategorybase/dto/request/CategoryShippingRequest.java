package com.gdn.x.productcategorybase.dto.request;

import com.gdn.x.productcategorybase.dto.BaseDTORequest;

public class CategoryShippingRequest extends BaseDTORequest {

  private static final long serialVersionUID = 1L;

  private String categoryCode;
  private ShippingRequest shippingCode;

  public CategoryShippingRequest() {
    // nothing to do here
  }

  public CategoryShippingRequest(String categoryCode, ShippingRequest shippingCode) {
    this.categoryCode = categoryCode;
    this.shippingCode = shippingCode;
  }

  public String getCategoryCode() {
    return this.categoryCode;
  }

  public ShippingRequest getShippingCode() {
    return this.shippingCode;
  }

  public void setCategoryCode(String categoryCode) {
    this.categoryCode = categoryCode;
  }

  public void setShippingCode(ShippingRequest shippingCode) {
    this.shippingCode = shippingCode;
  }

  @Override
  public String toString() {
    return String.format("CategoryShippingRequest [categoryCode=%s, shippingCode=%s, toString()=%s]", this.categoryCode,
        this.shippingCode, super.toString());
  }

}
