package com.gdn.x.productcategorybase.dto;

import java.io.Serializable;

public class UpdateImageDTO implements Serializable{

  private static final long serialVersionUID = -4366283202091085436L;
  
  private String imageCode;
  private String locationPath;
  
  public UpdateImageDTO() {
    
  }

  public String getImageCode() {
    return imageCode;
  }

  public UpdateImageDTO(String imageCode, String locationPath) {
    super();
    this.imageCode = imageCode;
    this.locationPath = locationPath;
  }

  public void setImageCode(String imageCode) {
    this.imageCode = imageCode;
  }

  public String getLocationPath() {
    return locationPath;
  }

  public void setLocationPath(String locationPath) {
    this.locationPath = locationPath;
  }

  @Override
  public String toString() {
    return String.format(
        "UpdateImageDTO [imageCode=%s, locationPath=%s, getImageCode()=%s, getLocationPath()=%s]",
        imageCode, locationPath, getImageCode(), getLocationPath());
  }
  
}
