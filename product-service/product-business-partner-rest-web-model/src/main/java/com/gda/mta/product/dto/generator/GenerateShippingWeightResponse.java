package com.gda.mta.product.dto.generator;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.web.base.BaseResponse;

@JsonIgnoreProperties(ignoreUnknown = true)
public class GenerateShippingWeightResponse extends BaseResponse {

  private static final long serialVersionUID = -75085497084440049L;
  private Double shippingWeight;
  
  public GenerateShippingWeightResponse(){}

  public GenerateShippingWeightResponse(Double shippingWeight) {
    super();
    this.shippingWeight = shippingWeight;
  }

  public Double getShippingWeight() {
    return shippingWeight;
  }

  public void setShippingWeight(Double shippingWeight) {
    this.shippingWeight = shippingWeight;
  }

  @Override
  public String toString() {
    return String.format("GenerateShippingWeightResponse [shippingWeight=%s]", shippingWeight);
  }

}
