package com.gdn.x.mta.distributiontask.service.api;

import com.gdn.x.mta.distributiontask.model.Product;
import com.gdn.x.mta.distributiontask.model.type.WorkflowState;

public interface QCTaskWrapperService {

  /**
   * To Reject product by QC
   *
   * @param product
   * @param reason
   * @param state
   * @param isAssignedToVendor
   */
  void qcRejectProduct(Product product, boolean isAssignedToVendor, String reason,
      WorkflowState state) throws Exception;
}
