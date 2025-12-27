package com.gdn.mta.bulk.service;

import com.gdn.mta.product.commons.constant.UpdateProductActivity;
import com.gdn.mta.product.entity.UpdatedProductHistory;

import java.util.List;

public interface UpdateProductHistoryService {

  /**
   * Get List of AuditTrialRequest and publish history event
   * @param auditTrailUpdatedProductList -> list of update product requests
   */
  void updateProductHistoryDetailList(List<UpdatedProductHistory> auditTrailUpdatedProductList, UpdateProductActivity activity);
}
