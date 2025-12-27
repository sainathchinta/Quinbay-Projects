package com.gdn.x.productcategorybase.dto;

import com.gdn.common.web.base.BaseRequest;

public class ActivateImageRequest extends BaseRequest{

  private static final long serialVersionUID = -5230299334546508105L;
  
  private String productCode;
  private String hashCode;
  private String filenames;
  private boolean commonImage;
  
  public ActivateImageRequest() {
    
  }
  
  public ActivateImageRequest(String productCode, String hashCode, String filenames) {
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
    return String
        .format(
            "ActivateImageRequest [productCode=%s, hashCode=%s, filenames=%s, getProductCode()=%s, getHashCode()=%s, getFilenames()=%s]",
            productCode, hashCode, filenames, getProductCode(), getHashCode(), getFilenames());
  }
  
  
  
}
