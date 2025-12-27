package com.gdn.mta.bulk.util;

import java.io.Serializable;
import java.util.Date;

public class BaseRequest implements Serializable {

  private static final long serialVersionUID = 8046144352036898563L;
  private String id;
  private String storeId;
  private Date createdDate;
  private String createdBy;
  private Date updatedDate;
  private String updatedBy;

  public BaseRequest() {
    // Do nothing
  }

  public BaseRequest(String id, String storeId, Date createdDate, String createdBy, Date updatedDate, String updatedBy) {
    super();
    this.id = id;
    this.storeId = storeId;
    this.createdDate = createdDate;
    this.createdBy = createdBy;
    this.updatedDate = updatedDate;
    this.updatedBy = updatedBy;
  }

  public String getCreatedBy() {
    return createdBy;
  }

  public Date getCreatedDate() {
    return createdDate;
  }

  public String getId() {
    return id;
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

  public void setCreatedBy(String createdBy) {
    this.createdBy = createdBy;
  }

  public void setCreatedDate(Date createdDate) {
    this.createdDate = createdDate;
  }

  public void setId(String id) {
    this.id = id;
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
            "BaseRequest [id=%s, storeId=%s, createdDate=%s, createdBy=%s, updatedDate=%s, updatedBy=%s, getId()=%s, getStoreId()=%s, getCreatedDate()=%s, getCreatedBy()=%s, getUpdatedDate()=%s, getUpdatedBy()=%s]",
            id, storeId, createdDate, createdBy, updatedDate, updatedBy, getId(), getStoreId(), getCreatedDate(),
            getCreatedBy(), getUpdatedDate(), getUpdatedBy());
  }

}
