package com.gdn.x.productcategorybase.dto.response;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

@JsonIgnoreProperties(ignoreUnknown = true)
public class ImagePathResponse {
  private String imagePath;
  private boolean mainImage;
  
  public ImagePathResponse() {
    super();
  }

  public ImagePathResponse(String imagePath, boolean mainImage) {
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
