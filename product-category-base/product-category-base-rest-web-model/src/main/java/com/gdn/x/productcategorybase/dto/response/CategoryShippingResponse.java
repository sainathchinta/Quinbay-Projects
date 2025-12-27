package com.gdn.x.productcategorybase.dto.response;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.x.productcategorybase.dto.BaseDTOResponse;

@JsonIgnoreProperties(ignoreUnknown = true)
public class CategoryShippingResponse extends BaseDTOResponse {

  private static final long serialVersionUID = 1L;

  private String categoryCode;
  private ShippingResponse shippingCode;

  public CategoryShippingResponse() {
    // nothing to do here
  }

  public CategoryShippingResponse(String categoryCode, ShippingResponse shippingCode) {
    super();
    this.categoryCode = categoryCode;
    this.shippingCode = shippingCode;
  }

  public String getCategoryCode() {
    return this.categoryCode;
  }

  public ShippingResponse getShippingCode() {
    return this.shippingCode;
  }

  public void setCategoryCode(String categoryCode) {
    this.categoryCode = categoryCode;
  }

  public void setShippingCode(ShippingResponse shippingCode) {
    this.shippingCode = shippingCode;
  }

  @Override
  public String toString() {
    return String.format("CategoryShippingResponse [categoryCode=%s, shippingCode=%s, toString()=%s]",
        this.categoryCode, this.shippingCode, super.toString());
  }


}
