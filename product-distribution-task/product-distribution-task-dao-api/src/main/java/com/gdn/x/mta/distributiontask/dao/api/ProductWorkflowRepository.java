package com.gdn.x.mta.distributiontask.dao.api;

import com.gdn.partners.pbp.dto.workflow.product.ProductWorkflowStatusResponse;

public interface ProductWorkflowRepository {
  /**
   * <p>Get PBP workflow states</p>
   * 
   * @param productCode
   * @return
   * @throws Exception 
   */
  ProductWorkflowStatusResponse getWorkflowStatus(String productCode) throws Exception;
}
