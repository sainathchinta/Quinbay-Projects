package com.gdn.x.productcategorybase.dto.brand;

import java.util.List;

import java.io.Serializable;

/**
 * Created by sarang on 08/06/17.
 */
public class BrandNamesRequest implements Serializable {

  private static final long serialVersionUID = -8763786088566342020L;
  private List<String> brandCodes;

  public BrandNamesRequest() {
  }

  public BrandNamesRequest(List<String> brandCodes) {
    this.brandCodes = brandCodes;
  }

  public List<String> getBrandCodes() {
    return brandCodes;
  }

  public void setBrandCodes(List<String> brandCodes) {
    this.brandCodes = brandCodes;
  }

  @Override public String toString() {
    final StringBuilder sb = new StringBuilder("BrandNamesRequest{");
    sb.append("brandCodes=").append(brandCodes);
    sb.append('}');
    return sb.toString();
  }

}
