package com.gdn.x.productcategorybase.dto;

import java.io.Serializable;

import org.apache.commons.lang3.builder.ToStringBuilder;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

/**
 * Created by Vishal on 12/06/18.
 */

@JsonIgnoreProperties(ignoreUnknown = true)
public class ActivateImageDTO implements Serializable{

  private static final long serialVersionUID = 7985845631289538074L;
  private String productCode;
  private String hashCode;
  private String filenames;
  private boolean commonImage;

  public ActivateImageDTO() {

  }

  public ActivateImageDTO(String productCode, String hashCode, String filenames) {
    super();
    this.productCode = productCode;
    this.hashCode = hashCode;
    this.filenames = filenames;
  }

  public String getProductCode() {
    return productCode;
  }

  public void setProductCode(String productCode) {
    this.productCode = productCode;
  }

  public String getHashCode() {
    return hashCode;
  }

  public void setHashCode(String hashCode) {
    this.hashCode = hashCode;
  }

  public String getFilenames() {
    return filenames;
  }

  public void setFilenames(String filenames) {
    this.filenames = filenames;
  }

  public boolean isCommonImage() {
    return commonImage;
  }

  public void setCommonImage(boolean commonImage) {
    this.commonImage = commonImage;
  }

  @Override
  public String toString() {
    return new ToStringBuilder(this).append("productCode", productCode).append("hashCode", hashCode)
        .append("filenames", filenames).toString();
  }
}
