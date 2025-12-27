package com.gdn.x.mta.distributiontask.rest.model.request;

import java.io.Serializable;

import com.gdn.x.mta.distributiontask.util.RejectReasonRequest;

/**
 * Created by virajjasani on 27/09/16.
 */
public class RejectProductVendorRequest implements Serializable {

  private static final long serialVersionUID = -8778322837000484006L;
  private String productCode;
  private String notes;
  private RejectReasonRequest rejectReasonRequest;
  private boolean isBulkAction;
  private String merchantCommissionType;

  public RejectProductVendorRequest() {
    // no implementation
  }

  public RejectProductVendorRequest(String productCode, String notes, RejectReasonRequest rejectReasonRequest) {
    this.productCode = productCode;
    this.notes = notes;
    this.rejectReasonRequest = rejectReasonRequest;
  }

  public RejectProductVendorRequest(String merchantCommissionType,
      RejectReasonRequest rejectReasonRequest, String notes, String productCode) {
    this.merchantCommissionType = merchantCommissionType;
    this.rejectReasonRequest = rejectReasonRequest;
    this.notes = notes;
    this.productCode = productCode;
  }

  public String getProductCode() {
    return productCode;
  }

  public void setProductCode(String productCode) {
    this.productCode = productCode;
  }

  public String getNotes() {
    return notes;
  }

  public void setNotes(String notes) {
    this.notes = notes;
  }

  public RejectReasonRequest getRejectReasonRequest() {
    return rejectReasonRequest;
  }

  public void setRejectReasonRequest(RejectReasonRequest rejectReasonRequest) {
    this.rejectReasonRequest = rejectReasonRequest;
  }

  public boolean isBulkAction() {
    return isBulkAction;
  }

  public void setBulkAction(boolean bulkAction) {
    isBulkAction = bulkAction;
  }

  public String getMerchantCommissionType() {
    return merchantCommissionType;
  }

  public void setMerchantCommissionType(String merchantCommissionType) {
    this.merchantCommissionType = merchantCommissionType;
  }

  @Override
  public String toString() {
    final StringBuilder sb = new StringBuilder("RejectProductVendorRequest{");
    sb.append("productCode='").append(productCode).append('\'');
    sb.append(", notes='").append(notes).append('\'');
    sb.append(", rejectReasonRequest='").append(rejectReasonRequest).append('\'');
    sb.append(", isBulkAction='").append(isBulkAction).append('\'');
    sb.append(", merchantCommissionType='").append(merchantCommissionType).append('\'');
    sb.append('}');
    return sb.toString();
  }
}
