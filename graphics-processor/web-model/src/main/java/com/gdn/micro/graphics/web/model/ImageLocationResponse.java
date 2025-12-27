package com.gdn.micro.graphics.web.model;

import com.gdn.common.web.base.BaseResponse;

/**
 * @author Yudhi K. Surtan
 *
 */
public class ImageLocationResponse extends BaseResponse {

  private static final long serialVersionUID = -3746252480971238165L;
  private String imageLocation;

  public ImageLocationResponse(String imageLocation) {
    this.imageLocation = imageLocation;
  }

  public String getImageLocation() {
    return imageLocation;
  }

  public void setImageLocation(String imageLocation) {
    this.imageLocation = imageLocation;
  }

  @Override
  public String toString() {
    StringBuilder builder = new StringBuilder();
    builder.append("ImageLocationResponse [imageLocation=").append(imageLocation).append("]");
    return builder.toString();
  }

}
