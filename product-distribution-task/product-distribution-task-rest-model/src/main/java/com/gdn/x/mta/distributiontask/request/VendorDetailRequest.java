package com.gdn.x.mta.distributiontask.request;

import com.gdn.x.mta.distributiontask.rest.model.base.DistributionBaseRequest;

import java.util.Date;

public class VendorDetailRequest extends DistributionBaseRequest{

  private String name;

  private String vendorCode;

  private Integer slaInDays;

  private Boolean isAbleToReject;

  private Boolean isQcRequired;

  private Date startHolidayDate;

  private Date endHolidayDate;

  private Integer quota;

  private String description;

  public VendorDetailRequest() {

  }

  private  VendorDetailRequest(Builder builder){
    this.name = builder.name;
    this.vendorCode = builder.vendorCode;
    this.slaInDays = builder.slaInDays;
    this.isAbleToReject=builder.isAbleToReject;
    this.isQcRequired=builder.isQcRequired;
    this.startHolidayDate=builder.startHolidayDate;
    this.endHolidayDate=builder.endHolidayDate;
    this.quota = builder.quota;
    this.description = builder.description;
  }

  public static class Builder {

    private String name;

    private String vendorCode;

    private Integer slaInDays;

    private Boolean isAbleToReject;

    private Boolean isQcRequired;

    private Date startHolidayDate;

    private Date endHolidayDate;

    private Integer quota;

    private String description;

    public VendorDetailRequest.Builder name(String name) {
      this.name = name;
      return this;
    }

    public VendorDetailRequest.Builder vendorCode(String vendorCode) {
      this.vendorCode = vendorCode;
      return this;
    }

    public VendorDetailRequest.Builder isAbleToReject(Boolean isAbleToReject) {
      this.isAbleToReject = isAbleToReject;
      return this;
    }

    public VendorDetailRequest.Builder slaInDays(Integer slaInDays) {
      this.slaInDays = slaInDays;
      return this;
    }

    public VendorDetailRequest.Builder isQcRequired(Boolean isQcRequired) {
      this.isQcRequired = isQcRequired;
      return this;
    }

    public VendorDetailRequest.Builder startHolidayDate(Date startHolidayDate) {
      this.startHolidayDate = startHolidayDate;
      return this;
    }

    public VendorDetailRequest.Builder endHolidayDate(Date endHolidayDate) {
      this.endHolidayDate = endHolidayDate;
      return this;
    }

    public VendorDetailRequest.Builder quota(Integer quota) {
      this.quota = quota;
      return this;
    }

    public VendorDetailRequest.Builder description(String description) {
      this.description = description;
      return this;
    }

    public VendorDetailRequest build() {
      return new VendorDetailRequest(this);
    }
  }

  public String getName() {
    return name;
  }

  public void setName(String name) {
    this.name = name;
  }

  public String getVendorCode() {
    return vendorCode;
  }

  public void setVendorCode(String vendorCode) {
    this.vendorCode = vendorCode;
  }

  public Boolean getAbleToReject() {
    return isAbleToReject;
  }

  public void setAbleToReject(Boolean ableToReject) {
    isAbleToReject = ableToReject;
  }

  public Integer getSlaInDays() {
    return slaInDays;
  }

  public void setSlaInDays(Integer slaInDays) {
    this.slaInDays = slaInDays;
  }

  public Boolean getQcRequired() {
    return isQcRequired;
  }

  public void setQcRequired(Boolean qcRequired) {
    isQcRequired = qcRequired;
  }

  public Date getStartHolidayDate() {
    return startHolidayDate;
  }

  public void setStartHolidayDate(Date startHolidayDate) {
    this.startHolidayDate = startHolidayDate;
  }

  public Date getEndHolidayDate() {
    return endHolidayDate;
  }

  public void setEndHolidayDate(Date endHolidayDate) {
    this.endHolidayDate = endHolidayDate;
  }

  public Integer getQuota() {
    return quota;
  }

  public void setQuota(Integer quota) {
    this.quota = quota;
  }

  public String getDescription() {
    return description;
  }

  public void setDescription(String description) {
    this.description = description;
  }

  @Override public String toString() {
    final StringBuilder sb = new StringBuilder("VendorDetailRequest{");
    sb.append("name='").append(name).append('\'');
    sb.append(", vendorCode='").append(vendorCode).append('\'');
    sb.append(", slaInDays=").append(slaInDays);
    sb.append(", isAbleToReject=").append(isAbleToReject);
    sb.append(", isQcRequired=").append(isQcRequired);
    sb.append(", startHolidayDate=").append(startHolidayDate);
    sb.append(", endHolidayDate=").append(endHolidayDate);
    sb.append(", quota=").append(quota);
    sb.append(", description='").append(description).append('\'');
    sb.append('}');
    return sb.toString();
  }
}
