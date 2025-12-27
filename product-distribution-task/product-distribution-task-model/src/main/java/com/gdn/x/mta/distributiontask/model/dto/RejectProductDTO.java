package com.gdn.x.mta.distributiontask.model.dto;

/**
 * Created by virajjasani on 27/09/16.
 */
public class RejectProductDTO {

  private String productCode;
  private String notes;
  private RejectReasonDto rejectReasonDto;
  private boolean isBulkAction;
  private String merchantCommissionType;
  private boolean isSchedulerAction;

  public RejectProductDTO() {
    // no implementation
  }

  public RejectProductDTO(String productCode, String notes, RejectReasonDto rejectReasonDto) {
    this.productCode = productCode;
    this.notes = notes;
    this.rejectReasonDto = rejectReasonDto;
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

  public RejectReasonDto getRejectReasonDto() {
    return rejectReasonDto;
  }

  public void setRejectReasonDto(RejectReasonDto rejectReasonDto) {
    this.rejectReasonDto = rejectReasonDto;
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

  public boolean isSchedulerAction() {
    return isSchedulerAction;
  }

  public void setSchedulerAction(boolean schedulerAction) {
    isSchedulerAction = schedulerAction;
  }

  @Override
  public String toString() {
    final StringBuilder sb = new StringBuilder("RejectProductDTO{");
    sb.append("productCode='").append(productCode).append('\'');
    sb.append(", notes='").append(notes).append('\'');
    sb.append(", rejectReasonRequest='").append(rejectReasonDto).append('\'');
    sb.append(", isBulkAction='").append(isBulkAction).append('\'');
    sb.append(", merchantCommissionType='").append(merchantCommissionType).append('\'');
    sb.append(", isSchedulerAction='").append(isSchedulerAction).append('\'');
    sb.append('}');
    return sb.toString();
  }
}
