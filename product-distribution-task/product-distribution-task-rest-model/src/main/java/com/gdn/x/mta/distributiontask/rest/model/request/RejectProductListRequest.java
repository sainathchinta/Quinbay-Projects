package com.gdn.x.mta.distributiontask.rest.model.request;

import java.io.Serializable;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

@JsonIgnoreProperties(ignoreUnknown = true)
public class RejectProductListRequest implements Serializable {
  private static final long serialVersionUID = -4428120865519973934L;

  private List<RejectProductVendorRequest> products;

  public RejectProductListRequest() {

  }

  public RejectProductListRequest(List<RejectProductVendorRequest> products) {
    this.products = products;
  }

  public List<RejectProductVendorRequest> getProducts() {
    return products;
  }

  public void setProducts(List<RejectProductVendorRequest> products) {
    this.products = products;
  }

  @Override
  public String toString() {
    return "RejectProductRequest{" + "products=" + products + '}';
  }
}
