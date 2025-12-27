package com.gdn.x.mta.distributiontask.rest.model.request;

import java.io.Serializable;

/**
 * Created by virajjasani on 01/10/16.
 */
public class VendorApprovalRequest implements Serializable {

  private static final long serialVersionUID = 4938671209734884979L;

  private String vendorCode;
  private String productCode;
  private String approve;

  public VendorApprovalRequest() {
    // no implementation
  }

  public VendorApprovalRequest(String vendorCode, String productCode, String approve) {
    this.vendorCode = vendorCode;
    this.productCode = productCode;
    this.approve = approve;
  }

  public String getVendorCode() {
    return vendorCode;
  }

  public void setVendorCode(String vendorCode) {
    this.vendorCode = vendorCode;
  }

  public String getProductCode() {
    return productCode;
  }

  public void setProductCode(String productCode) {
    this.productCode = productCode;
  }

  public String getApprove() {
    return approve;
  }

  public void setApprove(String approve) {
    this.approve = approve;
  }

  @Override
  public String toString() {
    final StringBuilder sb = new StringBuilder("VendorApprovalRequest{");
    sb.append("vendorCode='").append(vendorCode).append('\'');
    sb.append(", productCode='").append(productCode).append('\'');
    sb.append(", approve='").append(approve).append('\'');
    sb.append('}');
    return sb.toString();
  }
}
