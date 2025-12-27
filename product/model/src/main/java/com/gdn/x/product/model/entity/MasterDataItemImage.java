package com.gdn.x.product.model.entity;

import org.springframework.data.mongodb.core.mapping.Field;

import com.gdn.common.base.GdnObjects;
import com.gdn.x.product.enums.ProductFieldNames;

public class MasterDataItemImage implements GdnBaseEmbedded {

  private static final long serialVersionUID = 1L;

  @Field(value = ProductFieldNames.IS_MAIN_IMAGE)
  private boolean isMainImage;

  @Field(value = ProductFieldNames.LOCATION_PATH)
  private String locationPath;

  @Field(value = ProductFieldNames.SEQUENCE)
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
