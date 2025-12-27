package com.gdn.x.product.model.entity;

import org.springframework.data.mongodb.core.mapping.Field;

import com.gdn.common.base.GdnObjects;
import com.gdn.x.product.enums.ProductFieldNames;

public class MasterDataProductImage implements GdnBaseEmbedded {

  private static final long serialVersionUID = 1L;

  @Field(value = ProductFieldNames.IS_MAIN_IMAGE)
  private boolean isMainImage;

  @Field(value = ProductFieldNames.LOCATION_PATH)
  private String locationPath;

  @Field(value = ProductFieldNames.PRODUCT_CODE)
  private String productCode;

  @Field(value = ProductFieldNames.SEQUENCE)
  private int sequence;

  @Field(value = ProductFieldNames.COMMON_IMAGE)
  private boolean commonImage;

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

  public MasterDataProductImage(boolean isMainImage, String locationPath, String productCode,
    int sequence, boolean commonImage) {
    this.isMainImage = isMainImage;
    this.locationPath = locationPath;
    this.productCode = productCode;
    this.sequence = sequence;
    this.commonImage = commonImage;
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

  public boolean isCommonImage() {
    return commonImage;
  }

  public void setCommonImage(boolean commonImage) {
    this.commonImage = commonImage;
  }

  @Override
  public String toString() {
    return String.format(
      "MasterDataProductImage [isMainImage=%s, locationPath=%s, productCode=%s, sequence=%s, "
        + "commonImage=%s, toString()=%s]", this.isMainImage, this.locationPath, this.productCode,
      this.sequence, this.commonImage, super.toString());
  }

}
