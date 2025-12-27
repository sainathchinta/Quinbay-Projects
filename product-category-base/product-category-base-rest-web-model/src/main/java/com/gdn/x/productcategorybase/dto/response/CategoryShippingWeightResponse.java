package com.gdn.x.productcategorybase.dto.response;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.web.base.BaseResponse;

@JsonIgnoreProperties(ignoreUnknown = true)
public class CategoryShippingWeightResponse extends BaseResponse {

  private static final long serialVersionUID = -1503838738813840743L;
  private double shippingWeight;

  public double getShippingWeight() {
    return shippingWeight;
  }

  public void setShippingWeight(double shippingWeight) {
    this.shippingWeight = shippingWeight;
  }

  public CategoryShippingWeightResponse(double shippingWeight) {
    this.shippingWeight = shippingWeight;
  }

  public CategoryShippingWeightResponse() {
  }
}
