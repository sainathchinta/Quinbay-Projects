package com.gdn.mta.product.entity;


import java.io.Serializable;
import java.util.Date;
import java.util.Objects;
import lombok.AllArgsConstructor;

@AllArgsConstructor
public class ProductWfState implements Serializable {

  private static final long serialVersionUID = 3464060676202914571L;
  private String productCode;
  private String state;
  private Date createdDate;
  private Date updatedDate;
  private boolean postLive;

  public boolean isPostLive() {
    return postLive;
  }

  public void setPostLive(boolean postLive) {
    this.postLive = postLive;
  }

  public ProductWfState(String productCode, String state) {
    this.productCode = productCode;
    this.state = state;
  }

  public ProductWfState() {
  }

  public String getProductCode() {
    return productCode;
  }

  public void setProductCode(String productCode) {
    this.productCode = productCode;
  }

  public String getState() {
    return state;
  }

  public void setState(String state) {
    this.state = state;
  }

  public Date getCreatedDate() {
    return createdDate;
  }

  public String getCreatedDateAsString() {
    if(Objects.isNull(createdDate)){
      return "NULL";
    }
    return createdDate.toString();
  }

  public void setCreatedDate(Date createdDate) {
    this.createdDate = createdDate;
  }

  public Date getUpdatedDate() {
    return updatedDate;
  }

  public String getUpdatedDateAsString() {
    if(Objects.isNull(updatedDate)){
      return "NULL";
    }
    return createdDate.toString();
  }

  public void setUpdatedDate(Date updatedDate) {
    this.updatedDate = updatedDate;
  }

  public ProductWfState(String productCode, String state, boolean postLive) {
    this.productCode = productCode;
    this.state = state;
    this.postLive = postLive;
  }

  public ProductWfState(String productCode, String state, Date createdDate, Date updatedDate) {
    this.productCode = productCode;
    this.state = state;
    this.createdDate = createdDate;
    this.updatedDate = updatedDate;
  }

  @Override
  public String toString() {
    final StringBuilder sb = new StringBuilder("ProductWfStateResponse{");
    sb.append("productCode='").append(productCode).append('\'');
    sb.append(", state='").append(state).append('\'');
    sb.append(", postLive='").append(postLive).append('\'');
    sb.append(", createdDate='").append(createdDate).append('\'');
    sb.append(", updatedDate='").append(updatedDate).append('\'');
    sb.append('}');
    return sb.toString();
  }
}

