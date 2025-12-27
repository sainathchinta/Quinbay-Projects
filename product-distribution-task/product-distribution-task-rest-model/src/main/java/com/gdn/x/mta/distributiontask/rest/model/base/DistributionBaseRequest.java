package com.gdn.x.mta.distributiontask.rest.model.base;

import java.io.Serializable;
import java.util.Date;

/**
 * Created by virajjasani on 21/09/16.
 */
public class DistributionBaseRequest implements Serializable {

  private static final long serialVersionUID = -7799980119515674362L;

  private String id;
  private String storeId;
  private Date createdDate;
  private String createdBy;
  private Date updatedDate;
  private String updatedBy;
  private Long version;

  public DistributionBaseRequest() {
    // no implementation
  }

  public DistributionBaseRequest(String id, String storeId, Date createdDate, String createdBy,
      Date updatedDate, String updatedBy, Long version) {
    this.id = id;
    this.storeId = storeId;
    this.createdDate = createdDate;
    this.createdBy = createdBy;
    this.updatedDate = updatedDate;
    this.updatedBy = updatedBy;
    this.version = version;
  }

  public String getId() {
    return id;
  }

  public void setId(String id) {
    this.id = id;
  }

  public String getStoreId() {
    return storeId;
  }

  public void setStoreId(String storeId) {
    this.storeId = storeId;
  }

  public Date getCreatedDate() {
    return createdDate;
  }

  public void setCreatedDate(Date createdDate) {
    this.createdDate = createdDate;
  }

  public String getCreatedBy() {
    return createdBy;
  }

  public void setCreatedBy(String createdBy) {
    this.createdBy = createdBy;
  }

  public Date getUpdatedDate() {
    return updatedDate;
  }

  public void setUpdatedDate(Date updatedDate) {
    this.updatedDate = updatedDate;
  }

  public String getUpdatedBy() {
    return updatedBy;
  }

  public void setUpdatedBy(String updatedBy) {
    this.updatedBy = updatedBy;
  }

  public Long getVersion() {
    return version;
  }

  public void setVersion(Long version) {
    this.version = version;
  }

  @Override
  public String toString() {
    final StringBuilder sb = new StringBuilder("DistributionBaseRequest{");
    sb.append("id='").append(id).append('\'');
    sb.append(", storeId='").append(storeId).append('\'');
    sb.append(", createdDate=").append(createdDate);
    sb.append(", createdBy='").append(createdBy).append('\'');
    sb.append(", updatedDate=").append(updatedDate);
    sb.append(", updatedBy='").append(updatedBy).append('\'');
    sb.append(", version=").append(version);
    sb.append('}');
    return sb.toString();
  }
}
