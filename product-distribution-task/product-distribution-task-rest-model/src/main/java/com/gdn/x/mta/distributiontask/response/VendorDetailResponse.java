package com.gdn.x.mta.distributiontask.response;

import com.gdn.common.web.base.BaseResponse;

import java.util.Date;

/**
 * Created by Alok on 9/19/16.
 */
public class VendorDetailResponse extends BaseResponse{

  private static final long serialVersionUID = -8073445893413560160L;
  private String name;

  private String vendorCode;

  private Integer slaInDays;

  private Boolean isAbleToReject;

  private Boolean isQcRequired;

  private Date startHolidayDate;

  private Date endHolidayDate;

  private Integer quota;

  private String description;

  public VendorDetailResponse() {

  }

  private  VendorDetailResponse(VendorDetailResponse.Builder builder){
    this.name = builder.name;
    this.vendorCode = builder.vendorCode;
    this.slaInDays = builder.slaInDays;
    this.isAbleToReject=builder.isAbleToReject;
    this.isQcRequired=builder.isQcRequired;
    this.startHolidayDate=builder.startHolidayDate;
    this.endHolidayDate=builder.endHolidayDate;
    this.quota = builder.quota;
    this.description = builder.description;
    setId(builder.id);
  }

  public static class Builder {

    private String id;

    private String name;

    private String vendorCode;

    private Integer slaInDays;

    private Boolean isAbleToReject;

    private Boolean isQcRequired;

    private Date startHolidayDate;

    private Date endHolidayDate;

    private Integer quota;

    private String description;

    public VendorDetailResponse.Builder id(String id) {
      this.id = id;
      return this;
    }
    public VendorDetailResponse.Builder name(String name) {
      this.name = name;
      return this;
    }

    public VendorDetailResponse.Builder vendorCode(String vendorCode) {
      this.vendorCode = vendorCode;
      return this;
    }

    public VendorDetailResponse.Builder isAbleToReject(Boolean isAbleToReject) {
      this.isAbleToReject = isAbleToReject;
      return this;
    }

    public VendorDetailResponse.Builder slaInDays(Integer slaInDays) {
      this.slaInDays = slaInDays;
      return this;
    }

    public VendorDetailResponse.Builder isQcRequired(Boolean isQcRequired) {
      this.isQcRequired = isQcRequired;
      return this;
    }

    public VendorDetailResponse.Builder startHolidayDate(Date startHolidayDate) {
      this.startHolidayDate = startHolidayDate;
      return this;
    }

    public VendorDetailResponse.Builder endHolidayDate(Date endHolidayDate) {
      this.endHolidayDate = endHolidayDate;
      return this;
    }

    public VendorDetailResponse.Builder quota(Integer quota) {
      this.quota = quota;
      return this;
    }

    public VendorDetailResponse.Builder description(String description) {
      this.description = description;
      return this;
    }

    public VendorDetailResponse build() {
      return new VendorDetailResponse(this);
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

  @java.lang.Override
  public java.lang.String toString() {
    final java.lang.StringBuilder sb = new java.lang.StringBuilder("VendorDetailRequest{");
    sb.append("name='").append(name).append('\'');
    sb.append(", vendorCode='").append(vendorCode).append('\'');
    sb.append(", slaInDays=").append(slaInDays);
    sb.append(", isAbleToReject=").append(isAbleToReject);
    sb.append(", isQcRequired=").append(isQcRequired);
    sb.append(", startHolidayDate=").append(startHolidayDate);
    sb.append(", endHolidayDate=").append(endHolidayDate);
    sb.append(", quota=").append(quota);
    sb.append(", description=").append(description);
    sb.append('}');
    return sb.toString();
  }

}
