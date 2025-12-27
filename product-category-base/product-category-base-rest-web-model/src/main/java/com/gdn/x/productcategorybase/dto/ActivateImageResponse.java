package com.gdn.x.productcategorybase.dto;

import java.util.List;

import com.gdn.common.web.base.BaseResponse;

public class ActivateImageResponse extends BaseResponse{
  
  private static final long serialVersionUID = 734963979651208518L;
  
  private String productCode;
  private boolean active;
  private List<String> filenames;
  
  public ActivateImageResponse() {
    
  }
  
  public ActivateImageResponse(String productCode, boolean active, List<String> filenames) {
    super();
    this.productCode = productCode;
    this.active = active;
    this.filenames = filenames;
  }
  
  public String getProductCode() {
    return productCode;
  }

  public void setProductCode(String productCode) {
    this.productCode = productCode;
  }

  public boolean isActive() {
    return active;
  }

  public void setActive(boolean active) {
    this.active = active;
  }

  public List<String> getFilenames() {
    return filenames;
  }

  public void setFilenames(List<String> filenames) {
    this.filenames = filenames;
  }

  @Override
  public String toString() {
    return String
        .format(
            "ActivateImageResponse [productCode=%s, active=%s, filenames=%s, getProductCode()=%s, isActive()=%s, getFilenames()=%s]",
            productCode, active, filenames, getProductCode(), isActive(), getFilenames());
  }
  
}
