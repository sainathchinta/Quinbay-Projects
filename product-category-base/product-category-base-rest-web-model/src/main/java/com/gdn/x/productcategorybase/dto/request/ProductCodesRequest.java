package com.gdn.x.productcategorybase.dto.request;

import java.io.Serializable;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

@JsonIgnoreProperties(ignoreUnknown = true)
public class ProductCodesRequest implements Serializable {

  private static final long serialVersionUID = 2765615922246927139L;
  List<String> productCodes;

  public List<String> getProductCodes() {
    return productCodes;
  }

  public void setProductCodes(List<String> productCodes) {
    this.productCodes = productCodes;
  }
}
