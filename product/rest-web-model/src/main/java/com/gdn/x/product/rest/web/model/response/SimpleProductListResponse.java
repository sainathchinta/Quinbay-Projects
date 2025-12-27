package com.gdn.x.product.rest.web.model.response;

import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.GdnObjects;
import com.gdn.common.web.base.BaseResponse;
import com.gdn.x.product.rest.web.model.dto.SimpleProductDTO;

@JsonIgnoreProperties(ignoreUnknown = true)
public class SimpleProductListResponse extends BaseResponse {
  private static final long serialVersionUID = 1L;

  List<SimpleProductDTO> simpleProducts;

  public SimpleProductListResponse() {}

  public SimpleProductListResponse(List<SimpleProductDTO> simpleProducts) {
    super();
    this.simpleProducts = simpleProducts;
  }

  @Override
  public boolean equals(Object obj) {
    return GdnObjects.equals(this, obj);
  }

  public List<SimpleProductDTO> getSimpleProducts() {
    return this.simpleProducts;
  }

  @Override
  public int hashCode() {
    return GdnObjects.hashCode(this);
  }

  public void setSimpleProducts(List<SimpleProductDTO> simpleProducts) {
    this.simpleProducts = simpleProducts;
  }

  @Override
  public String toString() {
    return String.format("SimpleProductListResponse [simpleProducts=%s, toString()=%s]",
        this.simpleProducts, super.toString());
  }


}
