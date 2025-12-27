package com.gdn.x.product.rest.web.model.response;

import java.util.List;
import java.util.Map;


import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.GdnObjects;
import com.gdn.common.web.base.BaseResponse;

@JsonIgnoreProperties(ignoreUnknown = true)
public class ProductAndItemsAvailabilityResponse extends BaseResponse {

  private static final long serialVersionUID = 2185743805839971276L;

  private Map<String, List<ProductAndItemsResponse>> productAndItemsAvailability;

  public ProductAndItemsAvailabilityResponse() {}

  public ProductAndItemsAvailabilityResponse(
      Map<String, List<ProductAndItemsResponse>> productAndItemsAvailability) {
    this.productAndItemsAvailability = productAndItemsAvailability;
  }

  @Override
  public boolean equals(Object obj) {
    return GdnObjects.equals(this, obj);
  }

  public Map<String, List<ProductAndItemsResponse>> getProductAndItemsAvailability() {
    return productAndItemsAvailability;
  }

  @Override
  public int hashCode() {
    return GdnObjects.hashCode(this);
  }

  public void setProductAndItemsAvailability(
      Map<String, List<ProductAndItemsResponse>> productAndItemsAvailability) {
    this.productAndItemsAvailability = productAndItemsAvailability;
  }

  @Override
  public String toString() {
    StringBuilder builder = new StringBuilder();
    builder.append("ProductAndItemsAvailabilityResponse [productAndItemsAvailability=")
        .append(productAndItemsAvailability).append("]");
    return builder.toString();
  }

}
