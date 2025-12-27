package com.gdn.x.productcategorybase.dto;

import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

@JsonIgnoreProperties(ignoreUnknown = true)
public class GeneratedProductItemImagesPathDto {
  private String skuCode;
  private List<ImagePathDTO> images;
  
  public GeneratedProductItemImagesPathDto() {
    super();
  }
  
  public GeneratedProductItemImagesPathDto(String skuCode, List<ImagePathDTO> images) {
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

  public List<ImagePathDTO> getImages() {
    return images;
  }

  public void setImages(List<ImagePathDTO> images) {
    this.images = images;
  }
  
}
