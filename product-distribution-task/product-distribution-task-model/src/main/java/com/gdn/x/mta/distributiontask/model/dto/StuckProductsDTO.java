package com.gdn.x.mta.distributiontask.model.dto;

import java.util.Date;
import java.util.Objects;

import com.gdn.x.mta.distributiontask.model.type.WorkflowState;
import lombok.AllArgsConstructor;
import lombok.NoArgsConstructor;

@AllArgsConstructor
@NoArgsConstructor
public class StuckProductsDTO{
  private String productCode;
  private WorkflowState state;
  private Date createdDate;
  private Date updatedDate;

  public String getProductCode() {
    return productCode;
  }

  public void setProductCode(String productCode) {
    this.productCode = productCode;
  }

  public WorkflowState getState() {
    return state;
  }

  public String getStateAsString() {
    if(Objects.isNull(state)){
      return "NULL";
    }
    return state.toString();
  }

  public void setState(WorkflowState state) {
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
    return updatedDate.toString();
  }

  public void setUpdatedDate(Date updatedDate) {
    this.updatedDate = updatedDate;
  }

  @Override
  public String toString() {
    final StringBuilder sb = new StringBuilder("stuckProductsDTO{");
    sb.append("productCode='").append(productCode).append('\'');
    sb.append(", state='").append(state).append('\'');
    sb.append(", createdDate='").append(createdDate).append('\'');
    sb.append(", updatedDate='").append(updatedDate).append('\'');
    sb.append('}');
    return sb.toString();
  }
}