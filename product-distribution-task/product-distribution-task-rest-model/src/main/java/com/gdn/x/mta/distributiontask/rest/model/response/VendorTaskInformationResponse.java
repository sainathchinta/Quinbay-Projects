package com.gdn.x.mta.distributiontask.rest.model.response;

import com.gdn.common.web.base.BaseResponse;

public class VendorTaskInformationResponse extends BaseResponse {
  private static final long serialVersionUID = 7643686600854941822L;
  private String id;
  private String name;
  private String vendorCode;
  private Integer assignedCount;
  private Integer qcCount;
  private Integer capacity;

  public VendorTaskInformationResponse() {}

  public VendorTaskInformationResponse(String id, String name, String vendorCode,
      Integer assignedCount, Integer qcCount, Integer capacity) {
    super();
    this.id = id;
    this.name = name;
    this.vendorCode = vendorCode;
    this.assignedCount = assignedCount;
    this.qcCount = qcCount;
    this.capacity = capacity;
  }

  public Integer getAssignedCount() {
    return assignedCount;
  }

  public Integer getCapacity() {
    return capacity;
  }

  @Override
  public String getId() {
    return id;
  }

  public String getName() {
    return name;
  }

  public Integer getQcCount() {
    return qcCount;
  }

  public String getVendorCode() {
    return vendorCode;
  }

  public void setAssignedCount(Integer assignedCount) {
    this.assignedCount = assignedCount;
  }

  public void setCapacity(Integer capacity) {
    this.capacity = capacity;
  }

  @Override
  public void setId(String id) {
    this.id = id;
  }

  public void setName(String name) {
    this.name = name;
  }

  public void setQcCount(Integer qcCount) {
    this.qcCount = qcCount;
  }

  public void setVendorCode(String vendorCode) {
    this.vendorCode = vendorCode;
  }

  @Override
  public String toString() {
    StringBuilder builder = new StringBuilder();
    builder.append("VendorTaskInformationResponse [id=");
    builder.append(id);
    builder.append(", name=");
    builder.append(name);
    builder.append(", vendorCode=");
    builder.append(vendorCode);
    builder.append(", assignedCount=");
    builder.append(assignedCount);
    builder.append(", qcCount=");
    builder.append(qcCount);
    builder.append(", capacity=");
    builder.append(capacity);
    builder.append("]");
    return builder.toString();
  }
}
