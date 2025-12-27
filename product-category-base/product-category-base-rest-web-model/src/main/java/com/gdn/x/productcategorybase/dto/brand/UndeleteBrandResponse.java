package com.gdn.x.productcategorybase.dto.brand;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.web.base.BaseResponse;

@JsonIgnoreProperties(ignoreUnknown = true)
public class UndeleteBrandResponse extends BaseResponse {

  private static final long serialVersionUID = 4672377081715754225L;
  private String brandCode;

  public UndeleteBrandResponse() {}

  public UndeleteBrandResponse(String brandCode) {
    super();
    this.brandCode = brandCode;
  }

  public String getBrandCode() {
    return brandCode;
  }

  public void setBrandCode(String brandCode) {
    this.brandCode = brandCode;
  }

  @Override
  public String toString() {
    return String.format("UndeleteBrandResponse [brandCode=%s]", brandCode);
  }

}
