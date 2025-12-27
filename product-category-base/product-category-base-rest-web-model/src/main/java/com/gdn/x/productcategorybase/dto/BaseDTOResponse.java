package com.gdn.x.productcategorybase.dto;

import java.util.Date;

import com.gdn.common.base.GdnObjects;
import com.gdn.common.web.base.BaseResponse;

public abstract class BaseDTOResponse extends BaseResponse {

  private static final long serialVersionUID = -4871379715604867314L;

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

  @Override
  public String getCreatedBy() {
    return this.createdBy;
  }

  @Override
  public Date getCreatedDate() {
    return this.createdDate;
  }

  @Override
  public String getId() {
    return this.id;
  }

  @Override
  public String getStoreId() {
    return this.storeId;
  }

  @Override
  public String getUpdatedBy() {
    return this.updatedBy;
  }

  @Override
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

  @Override
  public void setCreatedBy(String createdBy) {
    this.createdBy = createdBy;
  }

  @Override
  public void setCreatedDate(Date createdDate) {
    this.createdDate = createdDate;
  }

  @Override
  public void setId(String id) {
    this.id = id;
  }

  public void setMarkForDelete(boolean markForDelete) {
    this.markForDelete = markForDelete;
  }

  @Override
  public void setStoreId(String storeId) {
    this.storeId = storeId;
  }

  @Override
  public void setUpdatedBy(String updatedBy) {
    this.updatedBy = updatedBy;
  }

  @Override
  public void setUpdatedDate(Date updatedDate) {
    this.updatedDate = updatedDate;
  }

  public void setVersion(Long version) {
    this.version = version;
  }

  @Override
  public String toString() {
    return String.format(
        "BaseDTOResponse [id=%s, storeId=%s, version=%s, createdDate=%s, updatedDate=%s, markForDelete=%s, toString()=%s]",
        this.id, this.storeId, this.version, this.createdDate, this.updatedDate, this.markForDelete, super.toString());
  }

}
