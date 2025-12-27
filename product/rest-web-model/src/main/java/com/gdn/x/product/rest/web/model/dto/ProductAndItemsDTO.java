package com.gdn.x.product.rest.web.model.dto;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;


import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.GdnObjects;
import com.gdn.x.product.rest.web.model.response.ItemResponse;
import com.gdn.x.product.rest.web.model.response.ProductResponse;

@JsonIgnoreProperties(ignoreUnknown = true)
public class ProductAndItemsDTO implements Serializable {

  private static final long serialVersionUID = 1L;

  private ProductResponse product;
  private List<ItemResponse> items = new ArrayList<ItemResponse>();

  public ProductAndItemsDTO() {

  }

  public ProductAndItemsDTO(ProductResponse product, List<ItemResponse> items) {
    super();
    this.product = product;
    this.items = items;
  }


  @Override
  public boolean equals(Object obj) {
    return GdnObjects.equals(this, obj);
  }

  public List<ItemResponse> getItems() {
    return this.items;
  }

  public ProductResponse getProduct() {
    return this.product;
  }

  @Override
  public int hashCode() {
    return GdnObjects.hashCode(this);
  }

  public void setItems(List<ItemResponse> items) {
    this.items = items;
  }

  public void setProduct(ProductResponse product) {
    this.product = product;
  }

  @Override
  public String toString() {
    return String.format("ProductAndItemsDTO [product=%s, items=%s, toString()=%s]", this.product,
        this.items, super.toString());
  }

}
