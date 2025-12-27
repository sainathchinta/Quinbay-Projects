package com.gda.mta.product.dto;

import com.gdn.common.web.base.BaseResponse;

public class UsageResponse extends BaseResponse{

  private static final long serialVersionUID = 8994872678211227642L;

  private boolean isUsed = false;

  public UsageResponse() {
    
  }
  
  public UsageResponse(boolean isUsed) {
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
    return String.format("UsageResponse [isUsed=%s, isUsed()=%s]", isUsed, isUsed());
  }
  
}
