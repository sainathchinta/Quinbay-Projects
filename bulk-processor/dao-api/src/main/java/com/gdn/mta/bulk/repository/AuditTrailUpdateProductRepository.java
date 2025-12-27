package com.gdn.mta.bulk.repository;

import java.util.List;

import com.gda.mta.product.dto.LogAuditTrailUpdatedProductBulkRequest;
import com.gda.mta.product.dto.LogAuditTrailUpdatedProductRequest;

public interface AuditTrailUpdateProductRepository {

  /**
   * create audit trail logs for updated products
   * @param logAuditTrailUpdatedProduct
   * @throws Exception
   */
  public void create(LogAuditTrailUpdatedProductBulkRequest logAuditTrailUpdatedProduct)
      throws Exception;
  
  public void create(List<LogAuditTrailUpdatedProductRequest> logAuditTrailUpdatedProduct)
      throws Exception;

}
