package com.gda.mta.product.dto.generator;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.web.base.BaseResponse;

import java.util.List;

@JsonIgnoreProperties(ignoreUnknown = true)
public class StuckProductResponse extends BaseResponse {

  private static final long serialVersionUID = -529031538350464439L;
  List<ProductWfStateResponse> productWfStateResponseList;
  int totalStuckProducts;
  List<ProductWfStateResponse> productsAboveRetryCount;

  public StuckProductResponse() {
    super();
  }

  public StuckProductResponse(List<ProductWfStateResponse> productWfStateResponseList,
      Integer totalStuckProducts,
      List<ProductWfStateResponse> productsAboveRetryCount) {
    this.productWfStateResponseList = productWfStateResponseList;
    this.totalStuckProducts = totalStuckProducts;
    this.productsAboveRetryCount = productsAboveRetryCount;
  }

  public StuckProductResponse(List<ProductWfStateResponse> productWfStateResponseList,
      Integer totalStuckProducts) {
    this.productWfStateResponseList = productWfStateResponseList;
    this.totalStuckProducts = totalStuckProducts;
  }

  public List<ProductWfStateResponse> getProductWfStateResponseList() {
    return productWfStateResponseList;
  }

  public void setProductWfStateResponseList(List<ProductWfStateResponse> productWfStateResponseList) {
    this.productWfStateResponseList = productWfStateResponseList;
  }

  public Integer getTotalStuckProducts() {
    return totalStuckProducts;
  }

  public void setTotalStuckProducts(Integer totalStuckProducts) {
    this.totalStuckProducts = totalStuckProducts;
  }

  public List<ProductWfStateResponse> getProductsAboveRetryCount() {
    return productsAboveRetryCount;
  }

  public void setProductsAboveRetryCount(List<ProductWfStateResponse> productsAboveRetryCount) {
    this.productsAboveRetryCount = productsAboveRetryCount;
  }

  @Override
  public String toString() {
    final StringBuilder sb = new StringBuilder("StuckProductResponse{");
    sb.append("productWfStateResponseList=").append(productWfStateResponseList);
    sb.append(", totalStuckProducts=").append(totalStuckProducts);
    sb.append(", productsAboveRetryCount=").append(productsAboveRetryCount);
    sb.append('}');
    return sb.toString();
  }
}
