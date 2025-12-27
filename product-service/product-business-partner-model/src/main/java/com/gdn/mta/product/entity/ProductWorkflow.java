package com.gdn.mta.product.entity;

import java.util.Date;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.Table;

import com.gdn.GdnBaseEntity;

@Entity
@Table(name = ProductWorkflow.TABLE_NAME)
public class ProductWorkflow extends GdnBaseEntity {

  private static final long serialVersionUID = 4092414755659374798L;
  public static final String TABLE_NAME = "PRD_PRODUCT_WORKFLOW";
  public static final String COLUMN_PRODUCT_ID = "PRODUCT_ID";
  public static final String COLUMN_STATE = "STATE";
  public static final String COLUMN_DESCRIPTION = "DESCRIPTION";
  public static final String COLUMN_NOTES = "NOTES";

  @Column(name = COLUMN_PRODUCT_ID, nullable = false)
  private String productId;

  @Column(name = COLUMN_STATE, nullable = false)
  private Integer state;

  @Column(name = COLUMN_DESCRIPTION, nullable = false)
  private String description;

  @Column(name = COLUMN_NOTES)
  private String notes;

  public ProductWorkflow() {}

  public ProductWorkflow(String productId, Integer state, String description, String createdBy, Date createdDate,
      String storeId) {
    this.productId = productId;
    this.state = state;
    this.description = description;
    setCreatedBy(createdBy);
    setCreatedDate(createdDate);
    setStoreId(storeId);
  }

  public ProductWorkflow(String productId, Integer state, String description, String notes, String createdBy,
      Date createdDate, String storeId) {
    this.productId = productId;
    this.state = state;
    this.description = description;
    this.notes = notes;
    setCreatedBy(createdBy);
    setCreatedDate(createdDate);
    setStoreId(storeId);
  }

  @Override
  public boolean equals(Object obj) {
    if (this == obj)
      return true;
    if (!super.equals(obj))
      return false;
    if (getClass() != obj.getClass())
      return false;
    ProductWorkflow other = (ProductWorkflow) obj;
    if (description == null) {
      if (other.description != null)
        return false;
    } else if (!description.equals(other.description))
      return false;
    if (productId == null) {
      if (other.productId != null)
        return false;
    } else if (!productId.equals(other.productId))
      return false;
    if (state == null) {
      if (other.state != null)
        return false;
    } else if (!state.equals(other.state))
      return false;
    return true;
  }

  public String getDescription() {
    return description;
  }

  public String getNotes() {
    return notes;
  }

  public String getProductId() {
    return productId;
  }

  public Integer getState() {
    return state;
  }

  @Override
  public int hashCode() {
    final int prime = 31;
    int result = super.hashCode();
    result = prime * result + ((description == null) ? 0 : description.hashCode());
    result = prime * result + ((productId == null) ? 0 : productId.hashCode());
    result = prime * result + ((state == null) ? 0 : state.hashCode());
    return result;
  }

  public void setDescription(String description) {
    this.description = description;
  }

  public void setNotes(String notes) {
    this.notes = notes;
  }

  public void setProductId(String productId) {
    this.productId = productId;
  }

  public void setState(Integer state) {
    this.state = state;
  }

  @Override
  public String toString() {
    StringBuilder builder = new StringBuilder();
    builder.append("ProductWorkflow [productId=").append(productId).append(", state=").append(state)
    .append(", description=").append(description).append(", notes=").append(notes).append(", getNotes()=")
    .append(getNotes()).append(", getDescription()=").append(getDescription()).append(", getProductId()=")
    .append(getProductId()).append(", getState()=").append(getState()).append(", hashCode()=").append(hashCode())
    .append("]");
    return builder.toString();
  }

}
