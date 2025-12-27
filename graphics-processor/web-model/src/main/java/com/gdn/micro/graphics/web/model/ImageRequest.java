package com.gdn.micro.graphics.web.model;

import org.apache.commons.lang3.builder.ToStringBuilder;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.web.base.BaseRequest;

import lombok.AllArgsConstructor;
import lombok.NoArgsConstructor;

/**
 * Created by Vishal on 09/06/18.
 */

@JsonIgnoreProperties(ignoreUnknown = true)
@NoArgsConstructor
@AllArgsConstructor
public class ImageRequest extends BaseRequest {

  private static final long serialVersionUID = 264602434638018763L;
  private String imageName;
  private String absoluteImagePath;
  private String hashCode;
  private boolean isEdited;
  private boolean commonImage;

  public ImageRequest(String imageName, String imageNameWithOutGroupCode, String hashCode) {
    this.imageName = imageName;
    this.absoluteImagePath = imageNameWithOutGroupCode;
    this.hashCode = hashCode;
  }

  public String getImageName() {
    return imageName;
  }

  public void setImageName(String imageName) {
    this.imageName = imageName;
  }

  public String getAbsoluteImagePath() {
    return absoluteImagePath;
  }

  public void setAbsoluteImagePath(String absoluteImagePath) {
    this.absoluteImagePath = absoluteImagePath;
  }

  public String getHashCode() {
    return hashCode;
  }

  public void setHashCode(String hashCode) {
    this.hashCode = hashCode;
  }

  public boolean isEdited() {
    return isEdited;
  }

  public void setEdited(boolean edited) {
    isEdited = edited;
  }

  public boolean isCommonImage() {
    return commonImage;
  }

  public void setCommonImage(boolean commonImage) {
    this.commonImage = commonImage;
  }

  @Override
  public String toString() {
    return new ToStringBuilder(this).append("imageName", imageName).append("absoluteImagePath", absoluteImagePath)
        .append("hashCode", hashCode).append("isEdited", isEdited).toString();
  }
}
