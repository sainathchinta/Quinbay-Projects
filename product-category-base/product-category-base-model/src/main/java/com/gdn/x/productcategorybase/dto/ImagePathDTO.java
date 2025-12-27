package com.gdn.x.productcategorybase.dto;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

@JsonIgnoreProperties(ignoreUnknown = true)
public class ImagePathDTO {
  private String imagePath;
  private boolean mainImage;
  
  public ImagePathDTO() {
    super();
  }

  public ImagePathDTO(String imagePath, boolean mainImage) {
    super();
    this.imagePath = imagePath;
    this.mainImage = mainImage;
  }

  public String getImagePath() {
    return imagePath;
  }

  public void setImagePath(String imagePath) {
    this.imagePath = imagePath;
  }

  public boolean isMainImage() {
    return mainImage;
  }

  public void setMainImage(boolean mainImage) {
    this.mainImage = mainImage;
  }
}
