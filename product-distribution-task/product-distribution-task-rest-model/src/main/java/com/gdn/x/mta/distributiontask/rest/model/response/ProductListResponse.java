package com.gdn.x.mta.distributiontask.rest.model.response;

import com.gdn.common.web.base.BaseResponse;

import java.io.Serializable;
import java.util.List;

/**
 * Created by vishal on 07/05/17.
 */
public class ProductListResponse extends BaseResponse implements Serializable {
  private static final long serialVersionUID = -7561778342261046788L;
  private List<String> productList;

  public ProductListResponse(){
  }
  public ProductListResponse(List<String> productList) {
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
