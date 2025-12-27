package com.gdn.mta.domain.event.modal;

import java.io.Serializable;

import com.gdn.common.base.entity.GdnBaseDomainEventModel;
import com.gdn.x.productcategorybase.dto.response.ProductDetailResponse;
import com.gdn.x.productcategorybase.dto.response.ProductResponse;

public class ApproveProductResponse extends GdnBaseDomainEventModel implements Serializable {

  private static final long serialVersionUID = -2976936679616666832L;
  private String message;
  private ProductResponse productResponse;

  public ApproveProductResponse(String message, ProductResponse productResponse) {
    this.message = message;
    this.productResponse = productResponse;
  }

  public ApproveProductResponse() {
  }

  public String getMessage() {
    return message;
  }

  public void setMessage(String message) {
    this.message = message;
  }

  public ProductResponse getProductResponse() {
    return productResponse;
  }

  public void setProductResponse(ProductResponse productResponse) {
    this.productResponse = productResponse;
  }

  @Override
  public String toString() {
    return String
        .format("ApproveProductResponse [ProductResponse=%s, locationPath=%s]", productResponse, message);
  }
}
