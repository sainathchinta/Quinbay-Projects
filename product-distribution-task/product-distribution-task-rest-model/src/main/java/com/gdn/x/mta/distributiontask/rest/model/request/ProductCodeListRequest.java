package com.gdn.x.mta.distributiontask.rest.model.request;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.web.base.BaseRequest;

import java.io.Serializable;
import java.util.List;

/**
 * Created by Vishal on 11/05/17.
 */
@JsonIgnoreProperties(ignoreUnknown = true)
public class ProductCodeListRequest extends BaseRequest implements Serializable {
  private static final long serialVersionUID = 1641640927858002933L;
  private List<String> productList;

  public ProductCodeListRequest(){

  }

  public ProductCodeListRequest(List<String> productList) {
    super();
    this.productList = productList;
  }

  public void setProductList(List<String> productList) {
    this.productList = productList;
  }

  public List<String> getProductList() {
    return productList;
  }

  @Override
  public String toString() {
    StringBuilder builder = new StringBuilder();
    builder.append("productList =");
    builder.append(productList);
    return builder.toString();
  }
}
