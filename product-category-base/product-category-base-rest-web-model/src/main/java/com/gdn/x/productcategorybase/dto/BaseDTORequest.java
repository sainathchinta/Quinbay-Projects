package com.gdn.x.productcategorybase.dto;

import java.util.Date;

import com.gdn.common.base.GdnObjects;
import com.gdn.common.web.base.BaseRequest;

public abstract class BaseDTORequest extends BaseRequest {

  private static final long serialVersionUID = -555711733950853893L;

  private String id;
  private String storeId;
  private Long version;
  private Date createdDate;
  private String createdBy;
  private Date updatedDate;
  private String updatedBy;
  private boolean markForDelete;

  /*
   * (non-Javadoc)
   * @see java.lang.Object#equals(java.lang.Object)
   */
  @Override
  public boolean equals(Object obj) {
    return GdnObjects.equals(obj, this);
  }

  public String getCreatedBy() {
    return this.createdBy;
  }

  public Date getCreatedDate() {
    return this.createdDate;
  }

  public String getId() {
    return this.id;
  }

  public String getStoreId() {
    return this.storeId;
  }

  public String getUpdatedBy() {
    return this.updatedBy;
  }

  public Date getUpdatedDate() {
    return this.updatedDate;
  }

  public Long getVersion() {
    return this.version;
  }

  /*
   * (non-Javadoc)
   * @see java.lang.Object#hashCode()
   */
  @Override
  public int hashCode() {
    return GdnObjects.hashCode(this);
  }

  public boolean isMarkForDelete() {
    return this.markForDelete;
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

  public void setVersion(Long version) {
    this.version = version;
  }

  @Override
  public String toString() {
    return String.format(
        "BaseDTORequest [id=%s, storeId=%s, version=%s, createdDate=%s, updatedDate=%s, markForDelete=%s, toString()=%s]",
        this.id, this.storeId, this.version, this.createdDate, this.updatedDate, this.markForDelete, super.toString());
  }
}
