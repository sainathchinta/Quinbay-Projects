package com.gdn.mta.bulk.dto.product;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

@JsonIgnoreProperties(ignoreUnknown = true)
public class ProductLevel3ImageRequest extends BaseRequest {

  private static final long serialVersionUID = -4165214336872356970L;
  private Boolean mainImage;
  private Integer sequence;
  private String locationPath;

  public ProductLevel3ImageRequest() {}

  public ProductLevel3ImageRequest(Boolean mainImage, Integer sequence, String locationPath) {
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

  public static long getSerialversionuid() {
    return serialVersionUID;
  }

  @Override
  public String toString() {
    return String
        .format(
            "ProductLevel3ImageRequest [mainImage=%s, sequence=%s, locationPath=%s, getMainImage()=%s, getSequence()=%s, getLocationPath()=%s]",
            mainImage, sequence, locationPath, getMainImage(), getSequence(), getLocationPath());
  }
}
