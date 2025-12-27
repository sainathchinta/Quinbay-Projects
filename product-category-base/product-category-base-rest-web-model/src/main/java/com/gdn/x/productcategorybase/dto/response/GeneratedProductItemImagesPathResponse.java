package com.gdn.x.productcategorybase.dto.response;

import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

@JsonIgnoreProperties(ignoreUnknown = true)
public class GeneratedProductItemImagesPathResponse {
  private String skuCode;
  private List<ImagePathResponse> images;
  
  public GeneratedProductItemImagesPathResponse() {
    super();
  }
  
  public GeneratedProductItemImagesPathResponse(String skuCode, List<ImagePathResponse> images) {
    super();
    this.skuCode = skuCode;
    this.images = images;
  }

  public String getSkuCode() {
    return skuCode;
  }

  public void setSkuCode(String skuCode) {
    this.skuCode = skuCode;
  }

  public List<ImagePathResponse> getImages() {
    return images;
  }

  public void setImages(List<ImagePathResponse> images) {
    this.images = images;
  }
  
}
