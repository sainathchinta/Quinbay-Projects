package com.gdn.x.productcategorybase.entity;

import java.io.Serializable;

public class ImageUpdate implements Serializable {

  private static final long serialVersionUID = 5553959846729614765L;
  
  private String imageCode;
  private String locationPath;
  
  public ImageUpdate() {
    
  }
  
  public ImageUpdate(String imageCode, String locationPath) {
    super();
    this.imageCode = imageCode;
    this.locationPath = locationPath;
  }
  
  public String getImageCode() {
    return imageCode;
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
    StringBuilder builder = new StringBuilder();
    builder.append("ImageUpdate [imageCode=").append(imageCode).append(", locationPath=")
        .append(locationPath).append(", getImageCode()=").append(getImageCode())
        .append(", getLocationPath()=").append(getLocationPath()).append("]");
    return builder.toString();
  }
  
}
