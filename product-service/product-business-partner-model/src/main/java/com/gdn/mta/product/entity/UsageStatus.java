package com.gdn.mta.product.entity;

import java.io.Serializable;

public class UsageStatus implements Serializable {

  private static final long serialVersionUID = 8433494554855233633L;

  private boolean isUsed = false;
  
  public UsageStatus() {
    
  }
  
  public UsageStatus(boolean isUsed) {
    super();
    this.isUsed = isUsed;
  }

  public boolean isUsed() {
    return isUsed;
  }

  public void setUsed(boolean isUsed) {
    this.isUsed = isUsed;
  }

  @Override
  public String toString() {
    return String.format("UsageStatus [isUsed=%s, isUsed()=%s]", isUsed, isUsed());
  }
  
}
