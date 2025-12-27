package com.gdn.x.productcategorybase.dto;

/**
 * Created by sarang on 09/06/17.
 */
public class BrandDTO {

  private String brandCode;
  private String brandName;

  public BrandDTO() {
    //No implementation
  }

  public BrandDTO(String brandCode, String brandName) {
    this.brandCode = brandCode;
    this.brandName = brandName;
  }

  public String getBrandCode() {
    return brandCode;
  }

  public void setBrandCode(String brandCode) {
    this.brandCode = brandCode;
  }

  public String getBrandName() {
    return brandName;
  }

  public void setBrandName(String brandName) {
    this.brandName = brandName;
  }

  @Override
  public String toString() {
    final StringBuilder sb = new StringBuilder("BrandDTO{");
    sb.append("brandCode='").append(brandCode).append('\'');
    sb.append(", brandName='").append(brandName).append('\'');
    sb.append('}');
    return sb.toString();
  }
}
