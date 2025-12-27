package com.gda.mta.product.dto;

import com.gdn.x.product.rest.web.model.request.ItemViewConfigRequest;

public class ProductItemLevel3ViewConfigRequest extends ItemViewConfigRequest {

  private static final long serialVersionUID = 1L;
  
  private boolean off2OnActiveFlag;

  public boolean isOff2OnActiveFlag() {
    return off2OnActiveFlag;
  }

  public void setOff2OnActiveFlag(boolean off2OnActiveFlag) {
    this.off2OnActiveFlag = off2OnActiveFlag;
  }

}
