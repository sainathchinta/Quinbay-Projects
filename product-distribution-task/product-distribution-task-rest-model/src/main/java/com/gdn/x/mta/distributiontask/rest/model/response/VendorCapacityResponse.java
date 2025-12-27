package com.gdn.x.mta.distributiontask.rest.model.response;

import java.util.Date;

import com.gdn.common.web.base.BaseResponse;

public class VendorCapacityResponse extends BaseResponse {
  private static final long serialVersionUID = -681377255658532144L;
  private String id;
  private String vendorCode;
  private String name;
  private Date startHolidayDate;
  private Date endHolidayDate;
  private Integer remainingCapacity;
  
  public VendorCapacityResponse(){}
  
  public VendorCapacityResponse(String id, String vendorCode, String name, Date startHolidayDate,
      Date endHolidayDate, Integer remainingCapacity) {
    super();
    this.id = id;
    this.vendorCode = vendorCode;
    this.name = name;
    this.startHolidayDate = startHolidayDate;
    this.endHolidayDate = endHolidayDate;
    this.remainingCapacity = remainingCapacity;
  }
  
  public String getId() {
    return id;
  }
  public void setId(String id) {
    this.id = id;
  }
  public String getVendorCode() {
    return vendorCode;
  }
  public void setVendorCode(String vendorCode) {
    this.vendorCode = vendorCode;
  }
  public String getName() {
    return name;
  }
  public void setName(String name) {
    this.name = name;
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
  public Integer getRemainingCapacity() {
    return remainingCapacity;
  }
  public void setRemainingCapacity(Integer remainingCapacity) {
    this.remainingCapacity = remainingCapacity;
  }
  @Override
  public String toString() {
    StringBuilder builder = new StringBuilder();
    builder.append("VendorCapacityResponse [id=");
    builder.append(id);
    builder.append(", vendorCode=");
    builder.append(vendorCode);
    builder.append(", name=");
    builder.append(name);
    builder.append(", startHolidayDate=");
    builder.append(startHolidayDate);
    builder.append(", endHolidayDate=");
    builder.append(endHolidayDate);
    builder.append(", remainingCapacity=");
    builder.append(remainingCapacity);
    builder.append("]");
    return builder.toString();
  }
}
