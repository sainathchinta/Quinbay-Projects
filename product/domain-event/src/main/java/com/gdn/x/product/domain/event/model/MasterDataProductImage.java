package com.gdn.x.product.domain.event.model;

import java.io.Serializable;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.GdnObjects;

@JsonIgnoreProperties(ignoreUnknown = true)
public class MasterDataProductImage implements Serializable {

  private static final long serialVersionUID = -1046333419030394774L;

  private boolean isMainImage;

  private String locationPath;

  private String productCode;

  private int sequence;

  public MasterDataProductImage() {

  }

  public MasterDataProductImage(boolean isMainImage, String locationPath, String productCode,
      int sequence) {
    super();
    this.isMainImage = isMainImage;
    this.locationPath = locationPath;
    this.productCode = productCode;
    this.sequence = sequence;
  }

  @Override
  public boolean equals(Object obj) {
    return GdnObjects.equals(this, obj);
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

  @Override
  public String toString() {
    return String
        .format(
            "MasterDataProductImage [isMainImage=%s, locationPath=%s, productCode=%s, sequence=%s, toString()=%s]",
            this.isMainImage, this.locationPath, this.productCode, this.sequence, super.toString());
  }

}
