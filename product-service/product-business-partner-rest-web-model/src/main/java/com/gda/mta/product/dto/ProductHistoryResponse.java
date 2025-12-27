package com.gda.mta.product.dto;

import java.util.Date;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.web.base.BaseResponse;

@JsonIgnoreProperties(ignoreUnknown = true)
public class ProductHistoryResponse extends BaseResponse {
  private static final long serialVersionUID = -8187409040960449603L;

  private String productId;
  private Integer state;
  private String description;
  private String notes;

  public ProductHistoryResponse() {}

  public ProductHistoryResponse(String id, String storeId, Date createdDate, String createdBy, Date updatedDate,
      String updatedBy, String productId, Integer state, String description, String notes) {
    super(id, storeId, createdDate, createdBy, updatedDate, updatedBy);
    this.productId = productId;
    this.state = state;
    this.description = description;
    this.notes = notes;
  }

  public String getProductId() {
    return productId;
  }

  public void setProductId(String productId) {
    this.productId = productId;
  }

  public Integer getState() {
    return state;
  }

  public void setState(Integer state) {
    this.state = state;
  }

  public String getDescription() {
    return description;
  }

  public void setDescription(String description) {
    this.description = description;
  }

  public String getNotes() {
    return notes;
  }

  public void setNotes(String notes) {
    this.notes = notes;
  }
}
