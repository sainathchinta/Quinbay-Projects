package com.gdn.x.product.rest.web.model.dto;

import java.io.Serializable;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.GdnObjects;

@JsonIgnoreProperties(ignoreUnknown = true)
public class MasterDataProductImageDTO implements Serializable {
  private static final long serialVersionUID = 1L;

  private boolean isMainImage;
  private String locationPath;
  private String productCode;
  private int sequence;
  private boolean commonImage;

  public MasterDataProductImageDTO() {

  }

  public MasterDataProductImageDTO(boolean isMainImage, String locationPath, String productCode,
      int sequence) {
    this.isMainImage = isMainImage;
    this.locationPath = locationPath;
    this.productCode = productCode;
    this.sequence = sequence;
  }

  public MasterDataProductImageDTO(boolean isMainImage, String locationPath, String productCode,
    int sequence, boolean commonImage) {
    this.isMainImage = isMainImage;
    this.locationPath = locationPath;
    this.productCode = productCode;
    this.sequence = sequence;
    this.commonImage = commonImage;
  }

  @Override
  public boolean equals(Object anObject) {
    return GdnObjects.equals(this, anObject);
  }

  public String getLocationPath() {
    return this.locationPath;
  }

  public String getProductCode() {
    return this.productCode;
  }

  public int getSequence() {
    return this.sequence;
  }

  @Override
  public int hashCode() {
    return GdnObjects.hashCode(this);
  }

  public boolean isMainImage() {
    return this.isMainImage;
  }

  public void setLocationPath(String locationPath) {
    this.locationPath = locationPath;
  }

  public void setMainImage(boolean isMainImage) {
    this.isMainImage = isMainImage;
  }

  public void setProductCode(String productCode) {
    this.productCode = productCode;
  }

  public void setSequence(int sequence) {
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
    return String
        .format(
            "MasterDataProductImageDTO [isMainImage=%s, locationPath=%s, productCode=%s, "
              + "sequence=%s, commonImage=%s, toString()=%s]",
            this.isMainImage, this.locationPath, this.productCode, this.sequence,
          this.commonImage, super.toString());
  }

}
