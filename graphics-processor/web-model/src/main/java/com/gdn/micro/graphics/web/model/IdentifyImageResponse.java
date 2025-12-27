package com.gdn.micro.graphics.web.model;

import com.gdn.common.web.base.BaseResponse;

public class IdentifyImageResponse extends BaseResponse {

  private static final long serialVersionUID = 5730951422811257220L;

  private boolean image = false;
  private Integer width;
  private Integer height;
  private boolean containResolution = false;
  private Integer resolutionWidth;
  private Integer resolutionHeight;
  private Integer quality;

  public IdentifyImageResponse() {
    // nothing to do here
  }

  public Integer getHeight() {
    return height;
  }

  public Integer getQuality() {
    return quality;
  }

  public Integer getResolutionHeight() {
    return resolutionHeight;
  }

  public Integer getResolutionWidth() {
    return resolutionWidth;
  }

  public Integer getWidth() {
    return width;
  }

  public boolean isContainResolution() {
    return containResolution;
  }

  public boolean isImage() {
    return image;
  }

  public void setContainResolution(boolean containResolution) {
    this.containResolution = containResolution;
  }

  public void setHeight(Integer height) {
    this.height = height;
  }

  public void setImage(boolean image) {
    this.image = image;
  }

  public void setQuality(Integer quality) {
    this.quality = quality;
  }

  public void setResolutionHeight(Integer resolutionHeight) {
    this.resolutionHeight = resolutionHeight;
  }

  public void setResolutionWidth(Integer resolutionWidth) {
    this.resolutionWidth = resolutionWidth;
  }

  public void setWidth(Integer width) {
    this.width = width;
  }

}
