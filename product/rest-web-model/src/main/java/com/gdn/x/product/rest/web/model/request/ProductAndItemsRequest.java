package com.gdn.x.product.rest.web.model.request;

import java.util.List;


import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.GdnObjects;
import com.gdn.common.web.base.BaseRequest;
import com.gdn.x.product.rest.web.model.dto.ItemDTO;
import com.gdn.x.product.rest.web.model.dto.ProductDTO;

@JsonIgnoreProperties(ignoreUnknown = true)
public class ProductAndItemsRequest extends BaseRequest {
  private static final long serialVersionUID = 1L;

  private ProductDTO product;
  private List<ItemDTO> items;

  public ProductAndItemsRequest() {}

  public ProductAndItemsRequest(ProductDTO product, List<ItemDTO> items) {
    super();
    this.product = product;
    this.items = items;
  }


  @Override
  public boolean equals(Object obj) {
    return GdnObjects.equals(this, obj);
  }


  public List<ItemDTO> getItems() {
    return this.items;
  }


  public ProductDTO getProduct() {
    return this.product;
  }


  @Override
  public int hashCode() {
    return GdnObjects.hashCode(this);
  }


  public void setItems(List<ItemDTO> items) {
    this.items = items;
  }


  public void setProduct(ProductDTO product) {
    this.product = product;
  }

  @Override
  public String toString() {
    return String.format("ProductAndItemsRequest [product=%s, items=%s, toString()=%s]",
        this.product, this.items, super.toString());
  }

}
