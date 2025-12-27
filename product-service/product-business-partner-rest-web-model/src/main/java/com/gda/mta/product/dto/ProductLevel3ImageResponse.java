package com.gda.mta.product.dto;

import com.gdn.common.web.base.BaseResponse;

public class ProductLevel3ImageResponse extends BaseResponse {

  private static final long serialVersionUID = -7391255382969622954L;
  private Boolean mainImage;
  private Integer sequence;
  private String locationPath;
  
  public ProductLevel3ImageResponse() {
    // do nothing
  }

  public ProductLevel3ImageResponse(Boolean mainImage, Integer sequence, String locationPath) {
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
    return String
        .format(
            "ProductLevel3ImageResponse [mainImage=%s, sequence=%s, locationPath=%s, getMainImage()=%s, getSequence()=%s, getLocationPath()=%s]",
            mainImage, sequence, locationPath, getMainImage(), getSequence(), getLocationPath());
  }

}
