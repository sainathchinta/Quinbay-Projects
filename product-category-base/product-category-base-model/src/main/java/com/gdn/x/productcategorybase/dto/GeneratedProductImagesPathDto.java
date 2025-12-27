package com.gdn.x.productcategorybase.dto;

import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

@JsonIgnoreProperties(ignoreUnknown = true)
public class GeneratedProductImagesPathDto {
  private String productCode;
  private List<ImagePathDTO> images;
  private List<GeneratedProductItemImagesPathDto> productItems;
  
  public GeneratedProductImagesPathDto() {
    super();
  }
  
  public GeneratedProductImagesPathDto(String productCode, List<ImagePathDTO> images,
      List<GeneratedProductItemImagesPathDto> productItems) {
    super();
    this.productCode = productCode;
    this.images = images;
    this.productItems = productItems;
  }

  public List<GeneratedProductItemImagesPathDto> getProductItems() {
    return productItems;
  }

  public void setProductItems(List<GeneratedProductItemImagesPathDto> productItems) {
    this.productItems = productItems;
  }

  public String getProductCode() {
    return productCode;
  }

  public void setProductCode(String productCode) {
    this.productCode = productCode;
  }

  public List<ImagePathDTO> getImages() {
    return images;
  }

  public void setImages(List<ImagePathDTO> images) {
    this.images = images;
  }
  
}
