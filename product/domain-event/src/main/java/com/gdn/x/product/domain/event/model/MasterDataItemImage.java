package com.gdn.x.product.domain.event.model;

import java.io.Serializable;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.GdnObjects;

@JsonIgnoreProperties(ignoreUnknown = true)
public class MasterDataItemImage implements Serializable {

  private static final long serialVersionUID = 7415940373588489651L;

  private boolean isMainImage;

  private String locationPath;

  private int sequence;

  public MasterDataItemImage() {

  }

  public MasterDataItemImage(boolean isMainImage, String locationPath, int sequence) {
    super();
    this.isMainImage = isMainImage;
    this.locationPath = locationPath;
    this.sequence = sequence;
  }

  @Override
  public boolean equals(Object obj) {
    return GdnObjects.equals(this, obj);
  }

  public String getLocationPath() {
    return this.locationPath;
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

  public void setSequence(int sequence) {
    this.sequence = sequence;
  }

  @Override
  public String toString() {
    return String.format("ItemImage [isMainImage=%s, locationPath=%s, sequence=%s, toString()=%s]",
        this.isMainImage, this.locationPath, this.sequence, super.toString());
  }

}
