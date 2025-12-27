package com.gdn.mta.product.entity;

import java.util.Date;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.Table;

import com.gdn.GdnBaseEntity;

@Entity
@Table(name = ProductHistory.TABLE_NAME)
public class ProductHistory extends GdnBaseEntity {

  private static final long serialVersionUID = 5266220563596090951L;
  public static final String TABLE_NAME = "PRD_PRODUCT_HISTORY";
  public static final String COLUMN_PRODUCT_ID = "PRODUCT_ID";
  public static final String COLUMN_STATE = "STATE";
  public static final String COLUMN_DESCRIPTION = "DESCRIPTION";
  public static final String COLUMN_NOTES = "NOTES";
  public static final String COLUMN_NEED_CORRECTION_NOTES = "NEED_CORRECTION_NOTES";

  @Column(name = COLUMN_PRODUCT_ID, nullable = false)
  private String productId;

  @Column(name = COLUMN_STATE, nullable = false)
  private Integer state;

  @Column(name = COLUMN_DESCRIPTION, nullable = false)
  private String description;

  @Column(name = COLUMN_NOTES)
  private String notes;

  @Column(name = COLUMN_NEED_CORRECTION_NOTES)
  private String needCorrectionNotes;

  public ProductHistory() {}

  public ProductHistory(String productId, Integer state, String description, String createdBy, Date createdDate,
      String storeId) {
    this.productId = productId;
    this.state = state;
    this.description = description;
    setCreatedBy(createdBy);
    setCreatedDate(createdDate);
    setStoreId(storeId);
  }

  public ProductHistory(String productId, Integer state, String description, String notes, String createdBy,
      Date createdDate, String storeId) {
    this.productId = productId;
    this.state = state;
    this.description = description;
    this.notes = notes;
    setCreatedBy(createdBy);
    setCreatedDate(createdDate);
    setStoreId(storeId);
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

  public String getNeedCorrectionNotes() {
    return needCorrectionNotes;
  }

  public void setNeedCorrectionNotes(String needCorrectionNotes) {
    this.needCorrectionNotes = needCorrectionNotes;
  }

  @Override
  public String toString() {
    StringBuilder builder = new StringBuilder();
    builder.append("ProductHistory [productId=").append(productId).append(", state=").append(state)
    .append(", description=").append(description).append(", notes=").append(notes).append(", getNotes()=")
    .append(getNotes()).append(", getDescription()=").append(getDescription()).append(", getProductId()=")
        .append(getProductId()).append(", getState()=").append(getState()).append(", getNeedCorrectionNotes()=")
        .append(getNeedCorrectionNotes()).append("]");
    return builder.toString();
  }

}
