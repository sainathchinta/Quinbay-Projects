package com.gdn.x.mta.distributiontask.domain.event.model;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.entity.GdnBaseDomainEventModel;

@JsonIgnoreProperties(ignoreUnknown = true)
public class ProductForcedRollbackSLAExceedDomainEventModel extends GdnBaseDomainEventModel {
  private String taskCode;
  private String taskId;
  private String productCode;
  private String productId;

  public ProductForcedRollbackSLAExceedDomainEventModel() {}

  public ProductForcedRollbackSLAExceedDomainEventModel(String taskCode, String taskId,
      String productCode, String productId) {
    super();
    this.taskCode = taskCode;
    this.taskId = taskId;
    this.productCode = productCode;
    this.productId = productId;
  }

  public String getProductCode() {
    return productCode;
  }

  public String getProductId() {
    return productId;
  }

  public String getTaskCode() {
    return taskCode;
  };

  public String getTaskId() {
    return taskId;
  }

  public void setProductCode(String productCode) {
    this.productCode = productCode;
  }

  public void setProductId(String productId) {
    this.productId = productId;
  }

  public void setTaskCode(String taskCode) {
    this.taskCode = taskCode;
  }

  public void setTaskId(String taskId) {
    this.taskId = taskId;
  }

  @Override
  public String toString() {
    StringBuilder builder = new StringBuilder();
    builder.append("ProductForcedRollbackSLAExceedDomainEventModel [taskCode=");
    builder.append(taskCode);
    builder.append(", taskId=");
    builder.append(taskId);
    builder.append(", productCode=");
    builder.append(productCode);
    builder.append(", productId=");
    builder.append(productId);
    builder.append("]");
    return builder.toString();
  }
}
