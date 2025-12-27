package com.gdn.x.mta.distributiontask.request;

import java.io.Serializable;
import java.util.List;

/**
 * Created by Alok on 9/20/16.
 */
public class ProductDistributionTaskRequest implements Serializable {

  private String vendorCode;
  private List<String> productCodes;

  public ProductDistributionTaskRequest() {
  }

  public ProductDistributionTaskRequest(String vendorId, List<String> productCodes) {
    this.vendorCode = vendorId;
    this.productCodes = productCodes;
  }

  public String getVendorCode() {
    return vendorCode;
  }

  public void setVendorCode(String vendorCode) {
    this.vendorCode = vendorCode;
  }

  public List<String> getProductCodes() {
    return productCodes;
  }

  public void setProductCodes(List<String> productCodes) {
    this.productCodes = productCodes;
  }

  @Override public String toString() {
    final StringBuilder sb = new StringBuilder("ProductDistributionTaskRequest{");
    sb.append("vendorCode='").append(vendorCode).append('\'');
    sb.append(", productCodes=").append(productCodes);
    sb.append('}');
    return sb.toString();
  }
}
