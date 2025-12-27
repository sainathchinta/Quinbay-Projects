package com.gdn.partners.pbp.dto.workflow.product;

import java.io.Serializable;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.partners.pbp.dto.productlevel3.UpdateProductLevel3WipRequest;
import com.gdn.x.productcategorybase.dto.request.ProductRequest;

@JsonIgnoreProperties(ignoreUnknown = true)
public class ProductResubmitRequest implements Serializable {

  private static final long serialVersionUID = 4478573720269240857L;

  private ProductRequest productRequest;
  private UpdateProductLevel3WipRequest productLevel3WipRequest;

  public ProductResubmitRequest() {

  }

  public ProductResubmitRequest(ProductRequest productRequest, UpdateProductLevel3WipRequest productLevel3WipRequest) {
    this.productRequest = productRequest;
    this.productLevel3WipRequest = productLevel3WipRequest;
  }

  public ProductRequest getProductRequest() {
    return productRequest;
  }

  public void setProductRequest(ProductRequest productRequest) {
    this.productRequest = productRequest;
  }

  public UpdateProductLevel3WipRequest getProductLevel3WipRequest() {
    return productLevel3WipRequest;
  }

  public void setProductLevel3WipRequest(UpdateProductLevel3WipRequest productLevel3WipRequest) {
    this.productLevel3WipRequest = productLevel3WipRequest;
  }

  @Override
  public String toString() {
    return "ProductResubmitRequest{" + "productRequest=" + productRequest + ", productLevel3WipRequest=" + productLevel3WipRequest + '}';
  }
}
