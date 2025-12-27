package com.gdn.x.productcategorybase.domain.event.model;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

@JsonIgnoreProperties(ignoreUnknown = true)
public class ImageDomainEventModel {

  private boolean mainImage;
  private String locationPath;
  private Integer sequence;
  private boolean commonImage;

  public ImageDomainEventModel() {
    // do nothing
  }

  public ImageDomainEventModel(boolean mainImage, String locationPath, Integer sequence) {
    super();
    this.mainImage = mainImage;
    this.locationPath = locationPath;
    this.sequence = sequence;
  }

  public String getLocationPath() {
    return locationPath;
  }

  public Integer getSequence() {
    return sequence;
  }

  public boolean isMainImage() {
    return mainImage;
  }

  public void setLocationPath(String locationPath) {
    this.locationPath = locationPath;
  }

  public void setMainImage(boolean mainImage) {
    this.mainImage = mainImage;
  }

  public void setSequence(Integer sequence) {
    this.sequence = sequence;
  }

  public boolean isCommonImage() {
    return commonImage;
  }

  public void setCommonImage(boolean commonImage) {
    this.commonImage = commonImage;
  }

  @Override
  public String toString() {
    return String.format(
        "ImageDomainEventModel [mainImage=%s, locationPath=%s, sequence=%s, commonImage=%s, isMainImage()=%s, getLocationPath()=%s, getSequence()=%s, isCommonImage=%s]",
        mainImage, locationPath, sequence, commonImage, isMainImage(), getLocationPath(), getSequence(),
        isCommonImage());
  }

}
