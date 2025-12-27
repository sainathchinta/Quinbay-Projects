package com.gdn.mta.product.entity;

import java.util.List;

import com.gdn.common.web.base.BaseResponse;

public class ProductLevel3ImageBundle extends BaseResponse {
  
  private static final long serialVersionUID = -1688182160685445864L;

  private String gdnSku;
  private List<ProductLevel3Image> images;

  public ProductLevel3ImageBundle() {
    super();
  }

  public ProductLevel3ImageBundle(String gdnSku, List<ProductLevel3Image> images) {
    super();
    this.gdnSku = gdnSku;
    this.images = images;
  }

  public String getGdnSku() {
    return gdnSku;
  }

  public void setGdnSku(String gdnSku) {
    this.gdnSku = gdnSku;
  }

  public List<ProductLevel3Image> getImages() {
    return images;
  }

  public void setImages(List<ProductLevel3Image> images) {
    this.images = images;
  }

  @Override
  public String toString() {
    return "ProductLevel3ImageBundle [gdnSku=" + gdnSku + ", images=" + images + "]";
  }

}
