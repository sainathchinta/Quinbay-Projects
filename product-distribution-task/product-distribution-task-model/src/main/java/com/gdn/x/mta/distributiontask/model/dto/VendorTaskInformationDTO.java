package com.gdn.x.mta.distributiontask.model.dto;

public class VendorTaskInformationDTO {
  private String id;
  private String name;
  private String vendorCode;
  private Integer assignedCount;
  private Integer qcCount;
  private Integer capacity;

  public VendorTaskInformationDTO() {}

  public VendorTaskInformationDTO(String id, String vendorCode, String name, Integer assignedCount, Integer qcCount,
      Integer capacity) {
    super();
    this.id = id;
    this.name = name;
    this.vendorCode = vendorCode;
    this.assignedCount = assignedCount;
    this.qcCount = qcCount;
    this.capacity = capacity;
  }

  public String getVendorCode() {
    return vendorCode;
  }

  public void setVendorCode(String vendorCode) {
    this.vendorCode = vendorCode;
  }

  public Integer getAssignedCount() {
    return assignedCount;
  }

  public Integer getCapacity() {
    return capacity;
  }

  public String getId() {
    return id;
  }

  public String getName() {
    return name;
  }

  public Integer getQcCount() {
    return qcCount;
  }

  public void setAssignedCount(Integer assignedCount) {
    this.assignedCount = assignedCount;
  }

  public void setCapacity(Integer capacity) {
    this.capacity = capacity;
  }

  public void setId(String id) {
    this.id = id;
  }

  public void setName(String name) {
    this.name = name;
  }

  public void setQcCount(Integer qcCount) {
    this.qcCount = qcCount;
  }

  @Override
  public String toString() {
    StringBuilder builder = new StringBuilder();
    builder.append("VendorAssignationDTO [id=");
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
