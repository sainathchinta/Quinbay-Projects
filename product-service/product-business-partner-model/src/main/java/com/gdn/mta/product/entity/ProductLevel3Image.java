package com.gdn.mta.product.entity;

import java.io.Serializable;

import com.gdn.common.web.base.BaseResponse;

public class ProductLevel3Image extends BaseResponse implements Serializable {

  private static final long serialVersionUID = -6775687092954240579L;
  private Boolean mainImage;
  private Integer sequence;
  private String locationPath;
  
  public ProductLevel3Image() {
    // do nothing
  }

  public ProductLevel3Image(Boolean mainImage, Integer sequence, String locationPath) {
    super();
    this.mainImage = mainImage;
    this.sequence = sequence;
    this.locationPath = locationPath;
  }

  public Boolean getMainImage() {
    return mainImage;
  }

  public void setMainImage(Boolean mainImage) {
    this.mainImage = mainImage;
  }

  public Integer getSequence() {
    return sequence;
  }

  public void setSequence(Integer sequence) {
    this.sequence = sequence;
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
    builder.append("ProductLevel3Image [mainImage=").append(mainImage).append(", sequence=")
        .append(sequence).append(", locationPath=").append(locationPath)
        .append(", getMainImage()=").append(getMainImage()).append(", getSequence()=")
        .append(getSequence()).append(", getLocationPath()=").append(getLocationPath()).append("]");
    return builder.toString();
  }

}
