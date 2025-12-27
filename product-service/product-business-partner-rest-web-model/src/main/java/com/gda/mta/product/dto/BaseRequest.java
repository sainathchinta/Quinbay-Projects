package com.gda.mta.product.dto;

import java.util.Date;

import com.gdn.common.web.wrapper.request.SimpleRequestHolder;

public class BaseRequest extends SimpleRequestHolder {

  private static final long serialVersionUID = -1463484819853603755L;
  private String storeId;
  private Date createdDate;
  private String createdBy;
  private Date updatedDate;
  private String updatedBy;
  private boolean markForDelete;

  public BaseRequest() {}

  public BaseRequest(String id, String storeId, Date createdDate, String createdBy, Date updatedDate, String updatedBy,
      boolean markForDelete) {
    super(id);
    this.storeId = storeId;
    this.createdDate = createdDate;
    this.createdBy = createdBy;
    this.updatedDate = updatedDate;
    this.updatedBy = updatedBy;
    this.markForDelete = markForDelete;
  }

  public String getCreatedBy() {
    return createdBy;
  }

  public Date getCreatedDate() {
    return createdDate;
  }

  public String getStoreId() {
    return storeId;
  }

  public String getUpdatedBy() {
    return updatedBy;
  }

  public Date getUpdatedDate() {
    return updatedDate;
  }

  public boolean isMarkForDelete() {
    return markForDelete;
  }

  public void setCreatedBy(String createdBy) {
    this.createdBy = createdBy;
  }

  public void setCreatedDate(Date createdDate) {
    this.createdDate = createdDate;
  }

  public void setMarkForDelete(boolean markForDelete) {
    this.markForDelete = markForDelete;
  }

  public void setStoreId(String storeId) {
    this.storeId = storeId;
  }

  public void setUpdatedBy(String updatedBy) {
    this.updatedBy = updatedBy;
  }

  public void setUpdatedDate(Date updatedDate) {
    this.updatedDate = updatedDate;
  }

  @Override
  public String toString() {
    return String
        .format(
            "BaseRequest [storeId=%s, createdDate=%s, createdBy=%s, updatedDate=%s, updatedBy=%s, markForDelete=%s, getId()=%s, toString()=%s, getClass()=%s, hashCode()=%s]",
            storeId, createdDate, createdBy, updatedDate, updatedBy, markForDelete, getId(), super.toString(),
            getClass(), hashCode());
  }

}
