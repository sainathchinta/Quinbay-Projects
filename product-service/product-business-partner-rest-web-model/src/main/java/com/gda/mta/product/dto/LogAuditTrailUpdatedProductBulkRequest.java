package com.gda.mta.product.dto;

import java.util.ArrayList;
import java.util.List;

public class LogAuditTrailUpdatedProductBulkRequest extends BaseRequest {

  private static final long serialVersionUID = 4158707009477666515L;

  private List<LogAuditTrailUpdatedProductRequest> logAuditTrailUpdatedProductList = new ArrayList<LogAuditTrailUpdatedProductRequest>();

  public List<LogAuditTrailUpdatedProductRequest> getLogAuditTrailUpdatedProductList() {
    return logAuditTrailUpdatedProductList;
  }

  public void setLogAuditTrailUpdatedProductList(
      List<LogAuditTrailUpdatedProductRequest> logAuditTrailUpdatedProductList) {
    this.logAuditTrailUpdatedProductList = logAuditTrailUpdatedProductList;
  }

}
