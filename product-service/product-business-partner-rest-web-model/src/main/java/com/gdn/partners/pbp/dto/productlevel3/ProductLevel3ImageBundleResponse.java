package com.gdn.partners.pbp.dto.productlevel3;

import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gda.mta.product.dto.ProductLevel3ImageResponse;
import com.gdn.common.web.base.BaseResponse;

@JsonIgnoreProperties(ignoreUnknown = true)
public class ProductLevel3ImageBundleResponse extends BaseResponse {

  private static final long serialVersionUID = -8741194982534182479L;
  
  private String gdnSku;
  private List<ProductLevel3ImageResponse> images;

  public String getGdnSku() {
    return gdnSku;
  }

  public void setGdnSku(String gdnSku) {
    this.gdnSku = gdnSku;
  }

  public List<ProductLevel3ImageResponse> getImages() {
    return images;
  }

  public void setImages(List<ProductLevel3ImageResponse> images) {
    this.images = images;
  }

  @Override
  public String toString() {
    return "ProductLevel3ImageBundleResponse [gdnSku=" + gdnSku + ", images=" + images + "]";
  }

}
