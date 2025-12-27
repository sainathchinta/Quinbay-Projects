package com.gdn.x.mta.distributiontask.model;

import java.util.Date;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.Table;

@Table(name = "PDT_VENDOR")
@Entity
public class Vendor extends GdnBaseEntity {
  private static final long serialVersionUID = 1L;

  @Column(name = "NAME")
  private String name;

  @Column(name = "VENDOR_CODE", unique = true, nullable = false)
  private String vendorCode;

  @Column(name = "SLA_IN_DAYS")
  private Integer slaInDays;

  @Column(name = "IS_ABLE_TO_REJECT")
  private Boolean isAbleToReject;

  @Column(name = "IS_QC_REQUIRED")
  private Boolean isQcRequired;

  @Column(name = "START_HOLIDAY_DATE")
  private Date startHolidayDate;

  @Column(name = "END_HOLIDAY_DATE")
  private Date endHolidayDate;

  @Column(name = "QUOTA")
  private Integer quota;

  @Column(name = "DESCRIPTION")
  private String description;

  public Vendor() {}

  private  Vendor(Builder builder){
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

    public Vendor.Builder id(String id) {
      this.id = id;
      return this;
    }
    public Vendor.Builder name(String name) {
      this.name = name;
      return this;
    }

    public Vendor.Builder vendorCode(String vendorCode) {
      this.vendorCode = vendorCode;
      return this;
    }

    public Vendor.Builder isAbleToReject(Boolean isAbleToReject) {
      this.isAbleToReject = isAbleToReject;
      return this;
    }

    public Vendor.Builder slaInDays(Integer slaInDays) {
      this.slaInDays = slaInDays;
      return this;
    }

    public Vendor.Builder isQcRequired(Boolean isQcRequired) {
      this.isQcRequired = isQcRequired;
      return this;
    }

    public Vendor.Builder startHolidayDate(Date startHolidayDate) {
      this.startHolidayDate = startHolidayDate;
      return this;
    }

    public Vendor.Builder endHolidayDate(Date endHolidayDate) {
      this.endHolidayDate = endHolidayDate;
      return this;
    }

    public Vendor.Builder quota(Integer quota) {
      this.quota = quota;
      return this;
    }

    public Vendor.Builder description(String description) {
      this.description = description;
      return this;
    }

    public Vendor build() {
      return new Vendor(this);
    }
  }

  public Date getEndHolidayDate() {
    return endHolidayDate;
  }

  public String getName() {
    return name;
  }

  public Integer getQuota() {
    return quota;
  }

  public Integer getSlaInDays() {
    return slaInDays;
  }

  public Date getStartHolidayDate() {
    return startHolidayDate;
  }

  public String getVendorCode() {
    return vendorCode;
  }

  public boolean isAbleToReject() {
    return isAbleToReject;
  }

  public boolean isQcRequired() {
    return isQcRequired;
  }

  public void setAbleToReject(Boolean isAbleToReject) {
    this.isAbleToReject = isAbleToReject;
  }

  public void setEndHolidayDate(Date endHolidayDate) {
    this.endHolidayDate = endHolidayDate;
  }

  public void setName(String name) {
    this.name = name;
  }

  public void setQcRequired(Boolean isQcRequired) {
    this.isQcRequired = isQcRequired;
  }

  public void setQuota(Integer quota) {
    this.quota = quota;
  }

  public void setSlaInDays(Integer slaInDays) {
    this.slaInDays = slaInDays;
  }

  public void setStartHolidayDate(Date startHolidayDate) {
    this.startHolidayDate = startHolidayDate;
  }

  public void setVendorCode(String vendorCode) {
    this.vendorCode = vendorCode;
  }

  public String getDescription() {
    return description;
  }

  public void setDescription(String description) {
    this.description = description;
  }

  @Override
  public String toString() {
    StringBuilder builder = new StringBuilder();
    builder.append("Vendor [name=");
    builder.append(name);
    builder.append(", vendorCode=");
    builder.append(vendorCode);
    builder.append(", slaInDays=");
    builder.append(slaInDays);
    builder.append(", isAbleToReject=");
    builder.append(isAbleToReject);
    builder.append(", isQcRequired=");
    builder.append(isQcRequired);
    builder.append(", startHolidayDate=");
    builder.append(startHolidayDate);
    builder.append(", endHolidayDate=");
    builder.append(endHolidayDate);
    builder.append(", quota=");
    builder.append(quota);
    builder.append(", description=");
    builder.append(description);
    builder.append(", toString=");
    builder.append(super.toString());
    builder.append("]");
    return builder.toString();
  }

}
