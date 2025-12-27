package com.gdn.x.productcategorybase.entity;

import java.io.Serializable;
import java.util.List;

public class ActivateImage implements Serializable{

  private static final long serialVersionUID = -4760466516287912533L;
  
  private String productCode;
  private boolean active;
  private List<String> filenames;
  
  public ActivateImage() {
    
  }
  
  public ActivateImage(String productCode, boolean active, List<String> filenames) {
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
            "ActivateImage [productCode=%s, active=%s, filenames=%s, getProductCode()=%s, isActive()=%s, getFilenames()=%s]",
            productCode, active, filenames, getProductCode(), isActive(), getFilenames());
  }
  
}
