package com.gdn.x.product.model.entity;

import java.io.Serializable;
import java.util.Date;

import com.fasterxml.jackson.annotation.JsonAlias;
import jakarta.persistence.GeneratedValue;
import org.hibernate.annotations.GenericGenerator;
import org.springframework.data.annotation.CreatedBy;
import org.springframework.data.annotation.CreatedDate;
import org.springframework.data.annotation.Id;
import org.springframework.data.annotation.LastModifiedBy;
import org.springframework.data.annotation.LastModifiedDate;
import org.springframework.data.annotation.Version;
import org.springframework.data.mongodb.core.mapping.Field;

import com.gdn.x.product.enums.ProductFieldNames;

public abstract class GdnBaseMongoEntity implements Serializable {

  private static final long serialVersionUID = 1L;

  @Id
  @GeneratedValue(generator = "system-uuid")
  @GenericGenerator(name = "system-uuid", strategy = "uuid2")
  private String id;

  @Version
  @Field(value = ProductFieldNames.VERSION)
  private Long version;

  @CreatedDate
  @JsonAlias(ProductFieldNames.CREATED_DATE)
  @Field(value = ProductFieldNames.CREATED_DATE)
  private Date createdDate;

  @CreatedBy
  @JsonAlias(ProductFieldNames.CREATED_BY)
  @Field(value = ProductFieldNames.CREATED_BY)
  private String createdBy;

  @LastModifiedDate
  @JsonAlias(ProductFieldNames.UPDATED_DATE)
  @Field(value = ProductFieldNames.UPDATED_DATE)
  private Date updatedDate;

  @LastModifiedBy
  @JsonAlias(ProductFieldNames.UPDATED_BY)
  @Field(value = ProductFieldNames.UPDATED_BY)
  private String updatedBy;

  @Field(value = ProductFieldNames.STORE_ID)
  @JsonAlias(ProductFieldNames.STORE_ID)
  private String storeId;

  @Field(value = ProductFieldNames.MARK_FOR_DELETE)
  @JsonAlias(ProductFieldNames.MARK_FOR_DELETE)
  private boolean markForDelete;

  /*
   * (non-Javadoc)
   * 
   * @see java.lang.Object#equals(java.lang.Object)
   */
  @Override
  public boolean equals(Object obj) {
    if (this == obj) {
      return true;
    }
    if (obj == null) {
      return false;
    }
    if (this.getClass() != obj.getClass()) {
      return false;
    }
    GdnBaseMongoEntity other = (GdnBaseMongoEntity) obj;
    if (this.id == null) {
      if (other.id != null) {
        return false;
      }
    } else if (!this.id.equals(other.id)) {
      return false;
    }
    return true;
  }

  public String getCreatedBy() {
    return this.createdBy;
  }

  /**
   * @return the createdDate
   */
  public Date getCreatedDate() {
    return this.createdDate;
  }

  /**
   * @return the id
   */
  public String getId() {
    return this.id;
  }

  /**
   * @return the storeId
   */
  public String getStoreId() {
    return storeId;
  }

  public String getUpdatedBy() {
    return this.updatedBy;
  }

  /**
   * @return the updatedDate
   */
  public Date getUpdatedDate() {
    return this.updatedDate;
  }

  /**
   * @return the version
   */
  public Long getVersion() {
    return this.version;
  }

  /*
   * (non-Javadoc)
   * 
   * @see java.lang.Object#hashCode()
   */
  @Override
  public int hashCode() {
    final int prime = 31;
    int result = 1;
    result = (prime * result) + ((this.id == null) ? 0 : this.id.hashCode());
    return result;
  }

  public boolean isMarkForDelete() {
    return markForDelete;
  }

  public void setCreatedBy(String createdBy) {
    this.createdBy = createdBy;
  }

  /**
   * @param createdDate the createdDate to set
   */
  public void setCreatedDate(Date createdDate) {
    this.createdDate = createdDate;
  }

  /**
   * @param id the id to set
   */
  public void setId(String id) {
    this.id = id;
  }

  public void setMarkForDelete(boolean markForDelete) {
    this.markForDelete = markForDelete;
  }

  /**
   * @param storeId the storeId to set
   */
  public void setStoreId(String storeId) {
    this.storeId = storeId;
  }


  public void setUpdatedBy(String updatedBy) {
    this.updatedBy = updatedBy;
  }

  /**
   * @param updatedDate the updatedDate to set
   */
  public void setUpdatedDate(Date updatedDate) {
    this.updatedDate = updatedDate;
  }

  /**
   * @param version the version to set
   */
  public void setVersion(Long version) {
    this.version = version;
  }

  /*
   * (non-Javadoc)
   * 
   * @see java.lang.Object#toString()
   */
  @Override
  public String toString() {
    return String
        .format(
            "GdnBaseMongoEntity [id=%s, storeId=%s, version=%s, createdDate=%s, updatedDate=%s, markForDelete=%s, toString()=%s]",
            this.id, this.storeId, this.version, this.createdDate, this.updatedDate,
            this.markForDelete, super.toString());
  }

}
