package com.gdn.x.mta.distributiontask.model.dto;

import java.io.Serializable;
import java.util.Date;

/**
 * @author febryo.lesmana
 */

public class VendorCapacityDTO implements Serializable {
  private static final long serialVersionUID = 7549264636581727528L;
  private String id;
  private String vendorCode;
  private String name;
  private Date startHolidayDate;
  private Date endHolidayDate;
  private Integer remainingCapacity;
  
  public VendorCapacityDTO(){}
  
  public VendorCapacityDTO(String id, String vendorCode, String name, Date startHolidayDate,
      Date endHolidayDate, Integer remainingCapacity) {
    super();
    this.id = id;
    this.vendorCode = vendorCode;
    this.name = name;
    this.startHolidayDate = startHolidayDate;
    this.endHolidayDate = endHolidayDate;
    this.remainingCapacity = remainingCapacity;
  }

  public Integer getRemainingCapacity() {
    return remainingCapacity;
  }

  public void setRemainingCapacity(Integer remainingCapacity) {
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

  @Override
  public String toString() {
    StringBuilder builder = new StringBuilder();
    builder.append("VendorCapacityDTO [id=");
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
