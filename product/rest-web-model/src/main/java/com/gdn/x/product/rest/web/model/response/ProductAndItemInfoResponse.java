package com.gdn.x.product.rest.web.model.response;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.web.base.BaseResponse;

@JsonIgnoreProperties(ignoreUnknown = true)
public class ProductAndItemInfoResponse extends BaseResponse {

  private static final long serialVersionUID = 4752103171988261523L;

  private ProductInfoResponse product;
  private ItemInfoResponse item;

  public ProductAndItemInfoResponse() {}

  public ProductAndItemInfoResponse(ProductInfoResponse product, ItemInfoResponse item) {
    super();
    this.product = product;
    this.item = item;
  }

  public ItemInfoResponse getItem() {
    return item;
  }

  public ProductInfoResponse getProduct() {
    return product;
  }

  public void setItem(ItemInfoResponse item) {
    this.item = item;
  }

  public void setProduct(ProductInfoResponse product) {
    this.product = product;
  }

  @Override
  public String toString() {
    return String.format("ProductAndItemsInfoResponse [product=%s, item=%s, toString()=%s]",
        product, item, super.toString());
  }

}
