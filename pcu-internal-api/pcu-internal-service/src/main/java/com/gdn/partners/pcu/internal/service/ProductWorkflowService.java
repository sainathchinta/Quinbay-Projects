package com.gdn.partners.pcu.internal.service;

import com.gdn.partners.pbp.dto.workflow.product.ProductReturnForCorrectionRequest;

/**
 * Created by govind on 11/01/2019 AD.
 */
public interface ProductWorkflowService {

  /**
   * Product Return for Correction
   * @param productReturnForCorrectionRequest
   */
  void returnForCorrection(ProductReturnForCorrectionRequest productReturnForCorrectionRequest);
}
